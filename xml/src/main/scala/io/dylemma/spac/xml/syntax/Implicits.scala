package io.dylemma.spac.xml.syntax

import javax.xml.stream.events.{StartElement, XMLEvent}

import io.dylemma.spac.handlers.ContextTracker
import io.dylemma.spac.{ConsumableLike, FromHandlerFactory, SplitterApply}
import io.dylemma.spac.types.Stackable
import io.dylemma.spac.xml.{XMLContextTracker, XMLEvents, XMLParser, XMLResource, XMLSplitter}

/** Defines XML-specific instances for the core spac typeclasses. */
trait Implicits {

	/** A `io.dylemma.spac.types.Stackable` instance for XML, using `XMLEvent`
	  * as the input type, and `StartElement` as the context stack element type.
	  */
	implicit val xmlStackable: Stackable.Aux[XMLEvent, StartElement] = new Stackable[XMLEvent] {
		type StackElem = StartElement
		def isPush(elem: XMLEvent) = elem.isStartElement
		def isPop(elem: XMLEvent) = elem.isEndElement
		def asPush(elem: XMLEvent) = if(elem.isStartElement) Some(elem.asStartElement) else None
	}

	implicit def makeContextTracker: ContextTracker[XMLEvent, StartElement] = new XMLContextTracker

	/** Implicit evidence that an `XMLEvents` instance can be opened as a stream of `XMLEvent`.
	  */
	implicit val consumableLikeXMLEvents: ConsumableLike[XMLEvents, XMLEvent] = XMLEvents.consumableLike

	/** Implicit evidence that any value of type `T` can be opened as a stream of `XMLEvent`
	  * as long as T belongs to the [[XMLResource]] typeclass.
	  * @tparam T A resource type that can be treated as a stream
	  * @return Implicit evidence that `T` can be opened as a stream of `XMLEvent`
	  */
	implicit def consumableLikeXMLResource[T: XMLResource]: ConsumableLike[T, XMLEvent] = XMLResource.consumableLike[T]

	/** Implicit evidence that you can call `Splitter.apply` on a `ContextMatcher[StartElement, T]` to get an `XMLSplitter[T]`.
	  */
	implicit val xmlSplitterApply: SplitterApply[StartElement, XMLSplitter] = XMLSplitter

	implicit val xmlParserFromHandlerFactory: FromHandlerFactory[XMLEvent, XMLParser] = XMLParser.handlerFactoryConverter
}
