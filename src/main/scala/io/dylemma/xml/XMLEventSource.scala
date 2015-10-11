package io.dylemma.xml

import javax.xml.stream.XMLInputFactory
import javax.xml.stream.events.{ Comment => NativeComment, StartElement => NativeStartElem, XMLEvent }

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Try }

import play.api.libs.iteratee.{ Input, Enumerator }

/** Allows an `underlyingSource` to be treated as a source of `XMLEvent`s.
  * This class exposes a `foreach` method (in the usual scala style) that
  * manages the resource lifecycle of the underlying source, exposing events
  * that come from parsing that source.
  *
  * @param underlyingSource An object that contains XML data, e.g. a File or String.
  * @param inputFactory The XMLInputFactory instance responsible for creating
  *                     an XMLEventReader from the resource opened from the
  *                     `underlyingSource`
  * @tparam T A type belonging to the `AsInputStream` typeclass, e.g. File or String.
  */
case class XMLEventSource[T: AsInputStream](underlyingSource: T, inputFactory: XMLInputFactory) {

	def foreach(f: XMLEvent => ParseInstruction): Unit = {
		val provider = implicitly[AsInputStream[T]]
		val resource = provider.openResource(underlyingSource)
		try {

			val stream = provider.resourceToStream(resource)
			val eventStream = inputFactory.createXMLEventReader(stream)

			var shouldStop = false
			while (!shouldStop && eventStream.hasNext) {
				val event = eventStream.nextEvent
				shouldStop = f(event) == StopParsing
			}
		} finally {
			provider.closeResource(resource)
		}
	}

}

object XMLEventEnumerator {
	/*
	TODO: This is actually bad because it opens the stream on the apply method.
	It should instead be opening the stream when the stream starts being consumed
	 */
	def apply[T: AsInputStream](source: T, inputFactory: XMLInputFactory = XMLEventSource.defaultInputFactory)(implicit ec: ExecutionContext): Enumerator[XMLEvent] = {
		val provider = implicitly[AsInputStream[T]]
		val resource = provider.openResource(source)
		try {
			val stream = provider.resourceToStream(resource)
			val eventStream = inputFactory.createXMLEventReader(stream)

			Enumerator generateM {
				if(eventStream.hasNext){
					Future.successful(Some(eventStream.nextEvent))
				} else {
					Future.successful(None)
				}
			} onDoneEnumerating {
				println("Done enumerating; closing the stream")
				provider.closeResource(resource)
			}

		} catch {
			case e: Throwable =>
				println("Something went wrong; closing the stream")
				provider.closeResource(resource)
				throw e
		}

	}
}

/** Defines a convenience method for constructing an XMLEventSource.
  */
object XMLEventSource {

	def apply[T: AsInputStream](underlyingSource: T): XMLEventSource[T] = {
		XMLEventSource(underlyingSource, defaultInputFactory)
	}

	/** Return a new XMLInputFactory that creates readers that do not auto-replace
	  * EntityReferences (in an effort to guard against XXE injections)
	  */
	def defaultInputFactory: XMLInputFactory = {
		val factory = XMLInputFactory.newInstance
		factory.setProperty(XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES, false)
		factory.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false)
		factory
	}
}