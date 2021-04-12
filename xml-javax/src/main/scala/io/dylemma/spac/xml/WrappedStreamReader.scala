package io.dylemma.spac
package xml

import javax.xml.namespace.QName
import javax.xml.stream.{Location, XMLStreamException, XMLStreamReader}

/** Used internally by `JavaxSource` as part of the conversion from a "source" to a `Stream[F, XmlEvent]`.
  */
private[xml] class WrappedStreamReader(reader: XMLStreamReader) extends Iterator[XmlEvent] with AutoCloseable {
	def close(): Unit = reader.close()
	def hasNext: Boolean = {
		while(nextEvent.isEmpty && reader.hasNext) tryAdvanceToNext()
		nextEvent.isDefined
	}
	def next(): XmlEvent = {
		while(nextEvent.isEmpty && reader.hasNext) tryAdvanceToNext()
		nextEvent match {
			case Some(e) =>
				nextEvent = None
				e
			case None =>
				throw new NoSuchElementException
		}
	}

	private var nextEvent: Option[XmlEvent] = None
	private var lastCheckedLocation: Option[ContextLocation] = None

	private def updateCheckedLocation(f: => ContextLocation) = {
		val result = f
		lastCheckedLocation = Some(result)
		result
	}

	private def tryAdvanceToNext() = {
		try advanceToNext()
		catch { case e: XMLStreamException =>
			lastCheckedLocation match {
				case None => throw e
				case Some(loc) => throw SpacException.addTrace(e, SpacTraceElement.NearLocation(loc))
			}
		}
	}

	private def advanceToNext(): Unit = {
		if(reader.hasNext) {
			reader.next()
			if(reader.isStartElement) {
				val qName = reader.getName
				val attrsMap = collection.mutable.Map.empty[String, List[(QName, String)]]
				for(i <- 0 until reader.getAttributeCount) {
					val k = reader.getAttributeName(i)
					val v = reader.getAttributeValue(i)
					val prevList = attrsMap.getOrElse(k.getLocalPart, Nil)
					attrsMap(k.getLocalPart) = (k -> v) :: prevList
				}
				val loc = getContextLocation(reader.getLocation)
				nextEvent = Some(new ElemStartImpl(qName, attrsMap, loc))

			} else if(reader.isEndElement) {
				val qName = reader.getName
				val loc = getContextLocation(reader.getLocation)
				nextEvent = Some(new ElemEndImpl(qName, loc))

			} else if(reader.isCharacters) {
				val text = reader.getText
				val isWhiteSpace = reader.isWhiteSpace
				val loc = getContextLocation(reader.getLocation)
				nextEvent = Some(new XmlTextImpl(text, isWhiteSpace, loc))

			}
		}
	}



	private def getContextLocation(loc: Location): ContextLocation = updateCheckedLocation {
		var result = ContextLocation.empty
		if(loc == null) result
		else {
			val line = loc.getLineNumber
			if(line != -1) result = result.and(ContextLineNumber, line.toLong)

			val col = loc.getColumnNumber
			if(col != -1) result = result.and(ContextColumnOffset, col.toLong)

			val charOffset = loc.getCharacterOffset
			if(charOffset != -1) result = result.and(ContextCharOffset, charOffset.toLong)

			result
		}
	}
}