package io.dylemma.spac
package xml
package spac_javax

import cats.effect.{Resource, Sync}
import javax.xml.namespace.QName
import javax.xml.stream.{Location, XMLStreamReader}

object WrappedStreamReader {
	def resourceAsXmlPull[F[+_]](resource: Resource[F, WrappedStreamReader])(implicit F: Sync[F]): Resource[F, Pullable[F, XmlEvent]] = resource
		.map[F[*], Pullable[F, XmlEvent]] { reader => // the type hint on `.map` seems to be necessary in scala 2.12
			new Pullable[F, XmlEvent] { self =>
				def uncons: F[Option[(XmlEvent, Pullable[F, XmlEvent])]] = F.delay {
					if(reader.hasNext) Some(reader.next() -> self)
					else None
				}
			}
		}
}

class WrappedStreamReader(reader: XMLStreamReader) extends Iterator[XmlEvent] with AutoCloseable {
	def close(): Unit = reader.close()
	def hasNext: Boolean = {
		while(nextEvent.isEmpty && reader.hasNext) advanceToNext()
		nextEvent.isDefined
	}
	def next(): XmlEvent = {
		while(nextEvent.isEmpty && reader.hasNext) advanceToNext()
		nextEvent match {
			case Some(e) =>
				nextEvent = None
				e
			case None =>
				throw new NoSuchElementException
		}
	}

	private var nextEvent: Option[XmlEvent] = None

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

	private def getContextLocation(loc: Location): ContextLocation = {
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