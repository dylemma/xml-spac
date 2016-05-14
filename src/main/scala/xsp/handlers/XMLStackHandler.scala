package xsp.handlers

import javax.xml.stream.events.{StartElement, XMLEvent}

import xsp.{Handler, Parser, Result}

case class XMLEventWithContext[Context](event: XMLEvent, context: Result[Context])
trait ContextMatcher[Context] {
	def matchContext(stack: Array[StartElement], offset: Int, length: Int): Result[Context]
}

class XMLStackHandler[Context, P](
	matcher: ContextMatcher[Context],
	parser: Parser[Context, P]
) extends Handler[XMLEvent, Unit]{
	private var _finished = false
	def isFinished: Boolean = _finished
	def handleEnd(): Unit = { _finished = true }

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

	def handleInput(input: XMLEvent): Option[Unit] = {
		println(s"event: $input")
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
						case Result.Error(err) => Some(parser makeHandler err)
						case _ => None // impossible, as we're in a !isEmpty check
					}
					println(s"Entered context: $newMatch")
				}
			}

			// feed the event to the current parser, if there is one
			if(currentParserHandler.isDefined){
				val handler = currentParserHandler.get
				if(!handler.isFinished){
					val feedResult = handler.handleInput(input)
					if(feedResult.isDefined){
						currentParserHandler = None
						println(s"Got inner parser result: ${feedResult.get}")
						// TODO: feed the result to the downstream handler
					}
				}
			}
		}

		// EndElement decreases the stack, and may close a context
		else if(input.isEndElement){
			popStack()

			// feed the event to the current parser, if there is one
			if(currentParserHandler.isDefined){
				val handler = currentParserHandler.get
				if(!handler.isFinished){
					val feedResult = handler.handleInput(input)
					if(feedResult.isDefined){
						currentParserHandler = None
						println(s"Got inner parser result: ${feedResult.get}")
						// TODO: feed the result to the downstream handler
					}
				}
			}

			// end the context if the stack goes below the place where the current context opened
			if(stackSize < matchStartDepth){
				matchStartDepth = 0
				println(s"Left context: $currentContext")
				currentContext = Result.Empty

				// if the context is ending, we need to feed an EOF to any unfinished parser
				if(currentParserHandler.isDefined){
					val handler = currentParserHandler.get
					if(!handler.isFinished){
						val feedResult = handler.handleEnd()
						currentParserHandler = None
						println(s"Got inner parser result: $feedResult")
						// TODO: feed the result to the downstream handler
					}
				}
			}
		}

		// other events don't affect the context
		else {
			if(currentParserHandler.isDefined){
				val handler = currentParserHandler.get
				if(!handler.isFinished){
					val feedResult = handler.handleInput(input)
					if(feedResult.isDefined){
						currentParserHandler = None
						println(s"Got inner parser result: ${feedResult.get}")
						// TODO: feed the result to the downstream handler
					}
				}
			}
		}

		// TODO: get the return value from the downstream handler
		None
	}

	def handleError(err: Throwable): Option[Unit] = {
		if(currentParserHandler.isDefined){
			val handler = currentParserHandler.get
			if(!handler.isFinished){
				val feedResult = handler.handleError(err)
				if(feedResult.isDefined){
					currentParserHandler = None
					println(s"Got inner parser result (from error): ${feedResult.get}")
					// TODO: feed the result to the downstream handler
				}
			}
		}

		// TODO: get the return value from the downstream handler
		None
	}
}
