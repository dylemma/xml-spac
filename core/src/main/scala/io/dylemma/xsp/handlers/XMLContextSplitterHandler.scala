package io.dylemma.xsp.handlers

import javax.xml.stream.events.{StartElement, XMLEvent}

import io.dylemma.xsp._

import scala.util.control.NonFatal

class XMLContextSplitterHandler[Context, P, Out](
	matcher: ContextMatcher[Context],
	makeInnerHandler: Context => Handler[XMLEvent, Result[P]], //Parser[Context, P],
	val downstream: Handler[P, Out]
) extends SplitterHandlerBase[XMLEvent, Context, P, Out]{
	override def toString = s"Splitter($matcher){ $makeInnerHandler } >> $downstream"

	lazy val debugName = s"Splitter($matcher)"
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

	def handleInput(input: XMLEvent): Option[Out] = {
		debug(s"event: $input")
		// StartElement increases the stack, and may open a new context
		if(input.isStartElement){
			val event = input.asStartElement
			pushStack(event)

			// attempt to match a context if there isn't one already
			if(currentContext.isEmpty){
				val newMatch = matcher(stackBuffer, 0, stackSize)
				if(!newMatch.isEmpty){
					currentContext = newMatch
					matchStartDepth = stackSize
					currentParserHandler = newMatch match {
						case Result.Success(ctx) => Some(makeInnerHandler(ctx))
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

}
