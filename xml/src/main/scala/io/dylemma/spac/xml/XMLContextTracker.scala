package io.dylemma.spac.xml

import javax.xml.stream.events.{StartElement, XMLEvent}

import io.dylemma.spac.ContextMatcher
import io.dylemma.spac.handlers.{ContextMove, ContextTracker}

import scala.collection.mutable.ArrayBuffer

class XMLContextTracker extends ContextTracker[XMLEvent, StartElement] {
	private var stack = new ArrayBuffer[StartElement](10)

	def currentDepth: Int = stack.length
	def copyStack = stack.toList
	def checkContext[A](matcher: ContextMatcher[StartElement, A]): Option[A] = matcher(stack, 0, stack.length)

	def evolve(event: XMLEvent): ContextMove = {
		if(event.isStartElement){
			stack += event.asStartElement
			ContextMove.pushThenPass
		} else if(event.isEndElement){
			stack.remove(stack.length - 1)
			ContextMove.passThenPop
		} else {
			ContextMove.Pass
		}
	}
}
