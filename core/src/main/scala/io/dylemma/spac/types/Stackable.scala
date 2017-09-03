package io.dylemma.spac.types

import javax.xml.stream.events.{StartElement, XMLEvent}

/** Generalization for types that can be interpreted as a "push" or "pop" to a stack.
  * For example, `XMLEvent` has `StartElement` and `EndElement` subclasses which can be
  * treated as "push" and "pop" respectively.
  */
trait Stackable[T] {
	type StackElem <: T
	def asPush(elem: T): Option[StackElem]
	def isPop(elem: T): Boolean
}

object Stackable {
	/** Stackable instance for XMLEvent, where the StackElem (`push`) type is StartElement,
	  * and EndElement events are treated as a `pop`.
 	  */
	implicit object xmlEventStackable extends Stackable[XMLEvent] {
		type StackElem = StartElement
		def asPush(elem: XMLEvent) = if(elem.isStartElement) Some(elem.asStartElement) else None
		def isPop(elem: XMLEvent) = elem.isEndElement
	}
}
