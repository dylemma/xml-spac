package io.dylemma.spac.old.xml.syntax

import io.dylemma.spac.old.{OldStackable, _}
import io.dylemma.spac.old.xml._
import javax.xml.stream.events.{StartElement, XMLEvent}

/** Defines XML-specific instances for the core spac typeclasses. */
trait Implicits {

	/** A `io.dylemma.spac.types.Stackable` instance for XML, using `XMLEvent`
	  * as the input type, and `StartElement` as the context stack element type.
	  */
	implicit val xmlStackable: OldStackable.Aux[XMLEvent, StartElement] = new OldStackable[XMLEvent] {
		type StackElem = StartElement
		def isPush(elem: XMLEvent) = elem.isStartElement
		def isPop(elem: XMLEvent) = elem.isEndElement
		def asPush(elem: XMLEvent) = if(elem.isStartElement) Some(elem.asStartElement) else None
		def order(elem: XMLEvent) = {
			if(elem.isStartElement) 1 // after context change, to include the StartElement in the new context
			else if(elem.isEndElement) -1 // before context change, to include the EndElement in the context it is ending
			else 0
		}
	}

	/** Implicit evidence that an `XMLEvents` instance can be opened as a stream of `XMLEvent`.
	  */
	implicit val consumableLikeXMLEvents: ConsumableLike[XMLEvents, XMLEvent] = XMLEvents.consumableLike

	/** Implicit evidence that any value of type `T` can be opened as a stream of `XMLEvent`
	  * as long as T belongs to the [[XMLResource]] typeclass.
	  * @tparam T A resource type that can be treated as a stream
	  * @return Implicit evidence that `T` can be opened as a stream of `XMLEvent`
	  */
	implicit def consumableLikeXMLResource[T: XMLResource]: ConsumableLike[T, XMLEvent] = XMLResource.consumableLike[T]

}
