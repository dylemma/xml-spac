package io.dylemma.spac.core

import javax.xml.stream.events.{StartElement, XMLEvent}

import io.dylemma.spac.ContextMatcher

trait Format[Event] {
	type StackElem
	def makeContextTracker: ContextTracker[Event, StackElem]
}

sealed trait ContextMove
object ContextMove {
	case object Pop extends ContextMove
	case object Push extends ContextMove
	case object Noop extends ContextMove
}

trait ContextTracker[Event, StackElem] {
	def evolve(event: Event): ContextMove
	def currentDepth: Int
	def checkContext[A](matcher: ContextMatcher[StackElem, A]): Option[A]
}

object Format {
	type Aux[Event, _0] = Format[Event]{ type StackElem = _0 }

	implicit object forXml extends Format[XMLEvent] {
		type StackElem = StartElement
		def makeContextTracker = new XmlContextTracker()
	}

	// TODO: Contextual[JsonEvent]
}

class XmlContextTracker extends ContextTracker[XMLEvent, StartElement] {
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

	def evolve(event: XMLEvent) = {
		if(event.isStartElement){
			pushStack(event.asStartElement)
			ContextMove.Push
		} else if(event.isEndElement){
			popStack()
			ContextMove.Pop
		} else {
			ContextMove.Noop
		}
	}

	def currentDepth = stackSize

	def checkContext[A](matcher: ContextMatcher[StartElement, A]) = matcher(stackBuffer, 0, stackSize)
}