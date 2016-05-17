package xsp.handlers

import javax.xml.stream.events.{StartElement, XMLEvent}

import xsp._

class XMLContextSplitterHandler[Context, P, Out](
	matcher: ContextMatcher[Context],
	parser: Parser[Context, P],
	downstream: Handler[P, Out]
) extends Handler[XMLEvent, Out]{

	def isFinished: Boolean = downstream.isFinished

	// ================
	// Stack Operations
	// ================
	private var stackBuffer = new Array[StartElement](10)
	private var stackSize = 0
	@inline private def expandStack() = {
		val newBuffer = new Array[StartElement](stackBuffer.length + 10)
		System.arraycopy(stackBuffer, 0, newBuffer, 0, stackSize)
		stackBuffer = newBuffer
	}
	@inline private def pushStack(elem: StartElement) = {
		if(stackSize + 1 >= stackBuffer.length) expandStack()
		stackBuffer(stackSize) = elem
		stackSize += 1
	}
	@inline private def popStack() = {
		if(stackSize > 0) stackSize -= 1
	}
	// =================

	private var currentContext: Result[Context] = Result.Empty
	private var matchStartDepth = 0
	private var currentParserHandler: Option[Handler[XMLEvent, Result[P]]] = None

	private def feedEndToCurrentParser(): Option[Result[P]] = {
		for {
			handler <- currentParserHandler
			if !handler.isFinished
		} yield {
			currentParserHandler = None
			handler.handleEnd()
		}
	}
	private def feedEventToCurrentParser(event: XMLEvent): Option[Result[P]] = {
		for {
			handler <- currentParserHandler
			if !handler.isFinished
			result <- handler.handleInput(event)
		} yield {
			currentParserHandler = None
			result
		}
	}
	private def feedErrorToCurrentParser(err: Throwable): Option[Result[P]] = {
		for {
			handler <- currentParserHandler
			if !handler.isFinished
			result <- handler.handleError(err)
		} yield {
			currentParserHandler = None
			result
		}
	}

	private def feedResultToDownstream(result: Result[P]): Option[Out] = {
		if(downstream.isFinished) None
		else result match {
			case Result.Success(p) => downstream.handleInput(p)
			case Result.Empty => None
			case Result.Error(err) => downstream.handleError(err)
		}
	}

	def handleInput(input: XMLEvent): Option[Out] = {
		debug(s"event: $input")
		// StartElement increases the stack, and may open a new context
		if(input.isStartElement){
			val event = input.asStartElement
			pushStack(event)

			// attempt to match a context if there isn't one already
			if(currentContext.isEmpty){
				val newMatch = matcher.matchContext(stackBuffer, 0, stackSize)
				if(!newMatch.isEmpty){
					currentContext = newMatch
					matchStartDepth = stackSize
					currentParserHandler = newMatch match {
						case Result.Success(ctx) => Some(parser makeHandler ctx)
						case e @ Result.Error(err) => Some(new OneShotHandler(e))
						case _ => None // impossible, as we're in a !isEmpty check
					}
					debug(s"Entered context: $newMatch")
				}
			}

			// feed the event to the current parser, if there is one
			feedEventToCurrentParser(input)
			  .map(debug as "Got inner parser result (from start element)")
			  .flatMap(feedResultToDownstream)
		}

		// EndElement decreases the stack, and may close a context
		else if(input.isEndElement){
			popStack()

			// feed the event to the current parser, if there is one
			val downstreamOutput = feedEventToCurrentParser(input)
			  .map(debug as "Got inner parser result (from context end)")
			  .flatMap(feedResultToDownstream)

			// as long as that last event didn't cause the parser to end,
			// we still need to feed an EOF to the parser
			downstreamOutput orElse {
				// end the context if the stack goes below the place where the current context opened
				if (stackSize < matchStartDepth) {
					matchStartDepth = 0
					debug(s"Left context: $currentContext")
					currentContext = Result.Empty

					// if the context is ending, we need to feed an EOF to any unfinished parser
					feedEndToCurrentParser()
					  .map(debug as "Got inner parser result (from post context end)")
					  .flatMap(feedResultToDownstream)
				} else {
					None
				}

			}
		}

		// other events don't affect the context
		else {
			feedEventToCurrentParser(input)
			  .map(debug as "Got inner parser result (from event)")
			  .flatMap(feedResultToDownstream)
		}
	}

	def handleError(err: Throwable): Option[Out] = {
		feedErrorToCurrentParser(err)
		  .map(debug as "Got inner parser result (from error)")
		  .flatMap(feedResultToDownstream)
	}
	def handleEnd(): Out = {
		feedEndToCurrentParser()
		  .map(debug as "Got inner parser result (from EOF)")
		  .flatMap(feedResultToDownstream)
		  .getOrElse{ downstream.handleEnd() }
	}
}
