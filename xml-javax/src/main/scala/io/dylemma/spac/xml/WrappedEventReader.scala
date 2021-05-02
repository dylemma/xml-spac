package io.dylemma.spac
package xml

import javax.xml.stream.{XMLEventReader, XMLStreamException}

private class WrappedEventReader(reader: XMLEventReader) extends Iterator[XmlEvent] with AutoCloseable {
	def close() = reader.close()

	def hasNext: Boolean = {
		while(nextEvent.isEmpty && reader.hasNext) advance()
		nextEvent.isDefined
	}
	def next(): XmlEvent = {
		while(nextEvent.isEmpty && reader.hasNext) advance()
		nextEvent match {
			case Some(e) =>
				nextEvent = None
				e
			case None =>
				throw new NoSuchElementException
		}
	}

	private var prevEvent: Option[XmlEvent] = None
	private var nextEvent: Option[XmlEvent] = None

	private def latestLocation: Option[ContextLocation] = nextEvent.orElse(prevEvent) map { _.location }

	private def advance(): Unit = {
		try {
			prevEvent = nextEvent
			nextEvent = WrappedJavaxXmlEvent(reader.nextEvent())
		} catch { case e: XMLStreamException =>
			latestLocation match {
				case None => throw e
				case Some(loc) => throw SpacException.addTrace(e, SpacTraceElement.NearLocation(loc))
			}
		}
	}
}