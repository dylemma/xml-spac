package io.dylemma.spac
package xml

import io.dylemma.spac.xml.JavaxQName.javaxQNameAsQName
import io.dylemma.spac.xml.XmlEvent._

import javax.xml.XMLConstants
import javax.xml.namespace.QName
import javax.xml.stream.events._
import javax.xml.stream.{Location, XMLStreamConstants}
import scala.annotation.switch

/**
  * @group support
  */
object JavaxQName {
	/** Allows `javax.xml.namespace.QName` to be passed to name-based methods like `elem` and `attr` */
	implicit val javaxQNameAsQName: AsQName[QName] = new AsQName[QName] {
		def name(n: QName): String = n.getLocalPart
		def namespaceUri(n: QName): Option[String] = n.getNamespaceURI match {
			case null | XMLConstants.NULL_NS_URI => None
			case s => Some(s)
		}
		def convert[N2](from: N2)(implicit N2: AsQName[N2]): QName = from match {
			case q: QName => q // just a cast instead of constructing a new instance
			case n2 => new QName(null, N2.name(n2), N2.namespaceUri(n2).getOrElse(XMLConstants.DEFAULT_NS_PREFIX))
		}
		def equals[N2](n: QName, r: N2)(implicit N2: AsQName[N2]): Boolean = r match {
			case q: QName => n == q
			case n2 => (n.getLocalPart == N2.name(n2)) && (n.getNamespaceURI == N2.namespaceUri(n2).getOrElse(XMLConstants.NULL_NS_URI))
		}
	}
}

private object JavaxLocation {
	def convert(loc: Location): ContextLocation = new Wrapper(loc)

	private class Wrapper(loc: Location) extends ContextLocation {
		import ContextLocation.Tag._
		def get[A](tag: ContextLocation.Tag[A]) = tag match {
			case LineNumber =>
				val line = loc.getLineNumber
				if (line == -1) None else Some(line.toLong)

			case ColumnOffset =>
				val col = loc.getColumnNumber
				if (col == -1) None else Some(col.toLong)

			case CharOffset =>
				val charOffset = loc.getCharacterOffset
				if (charOffset == -1) None else Some(charOffset.toLong)

			case _ => None
		}
	}
}
private class JavaxAttributesIterator[N](e: StartElement)(implicit N: AsQName[N]) extends Iterator[(N, String)] {
	private val inner = e.getAttributes
	def hasNext = inner.hasNext
	def next() = {
		val attr = inner.next().asInstanceOf[Attribute]
		N.convert(attr.getName) -> attr.getValue
	}
}

private object WrappedJavaxXmlEvent {
	def apply(e: XMLEvent) = (e.getEventType: @switch) match {
		case XMLStreamConstants.START_ELEMENT => Some(new WrappedJavaxStartElement(e.asStartElement))
		case XMLStreamConstants.END_ELEMENT => Some(new WrappedJavaxEndElement(e.asEndElement))
		case XMLStreamConstants.CHARACTERS => Some(new WrappedJavaxTextEvent(e.asCharacters))
		case _ => None
	}
}

private class WrappedJavaxStartElement(e: StartElement) extends ElemStart {
	def qName[N](implicit N: AsQName[N]) = N.convert(e.getName)
	def attr[N: AsQName](attributeQName: N) = {
		val attrOrNull = e.getAttributeByName(javaxQNameAsQName.convert(attributeQName))
		if (attrOrNull == null) None else Some(attrOrNull.getValue)
	}
	def attrs[N](implicit N: AsQName[N]): Iterator[(N, String)] = new JavaxAttributesIterator[N](e)
	def location = JavaxLocation.convert(e.getLocation)
}

private class WrappedJavaxEndElement(e: EndElement) extends ElemEnd {
	def qName[N](implicit N: AsQName[N]) = N.convert(e.getName)
	def location = JavaxLocation.convert(e.getLocation)
}

private class WrappedJavaxTextEvent(e: Characters) extends Text {
	def value = e.getData
	def isWhitespace = e.isWhiteSpace
	def location = JavaxLocation.convert(e.getLocation)
}