package xsp.handlers

import javax.xml.stream.events.{StartElement, XMLEvent}

import xsp.{Handler, Result}

case class XMLEventWithContext[Context](event: XMLEvent, context: Result[Context])
trait ContextMatcher[Context] {
	def matchContext(stack: Array[StartElement], offset: Int, length: Int): Result[Context]
}

class XMLStackHandler[Context](matcher: ContextMatcher[Context]) extends Handler[XMLEvent, Unit]{
	private var _finished = false
	def isFinished: Boolean = _finished
	def handleEnd(): Unit = { _finished = true }
	def handleError(err: Throwable): Option[Unit] = throw err

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
					println(s"Entered context: $newMatch")
				}
			}
		} else if(input.isEndElement){
			// EndElement decreases the stack, and may close a context
			popStack()
			if(stackSize < matchStartDepth){
				matchStartDepth = 0
				println(s"Left context: $currentContext")
				currentContext = Result.Empty
			}
		}

		// TODO: pass the event+context along to some downstream handler
		None
	}
}
