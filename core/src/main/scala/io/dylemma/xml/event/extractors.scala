package io.dylemma.xml.event

import javax.xml.namespace.QName
import javax.xml.stream.events.{ Attribute, Comment => NativeComment, StartElement => NativeStartElem, XMLEvent }

/** Contains a collection of Extractor objects that can be used on
  * `javax.xml.stream.events.XMLEvent`s and `javax.xml.namespace.QName`s.
  * Client code should import `Extractors._` for best effect.
  */

/** Extracts the `prefix` and `localName` (in that order) from a QName.
  * If the `prefix` is empty or null, the match fails.
  */
object PrefixName {
	def unapply(qname: QName): Option[(String, String)] = {
		val prefix = qname.getPrefix
		val name = qname.getLocalPart
		if (prefix != null && !prefix.isEmpty) {
			Some(prefix -> name)
		} else {
			None
		}
	}
}

/** Extracts the `localName` from a QName.
  * E.g. for a node like `<avi:foo>` it would extract "foo".
  * Use this extractor when you don't care about the prefix.
  */
object Name {
	def unapply(qname: QName): Option[String] = {
		Some(qname.getLocalPart)
	}
}

/** Interface describing a set of Attribute values. Attributes
  * are Strings that can be looked up by Name or QName.
  */
trait Attributes extends Iterable[(QName, String)] {
	def get(qname: QName): Option[String]
	def get(name: String): Option[String] = get(new QName(name))
	def iterator: Iterator[(QName, String)]

	override def toString = {
		iterator.map {
			case (k, v) => s"$k='$v'"
		}.mkString("Attributes(", ", ", ")")
	}
}

/*
 * Implementation for Attributes that delegates a StartElement event for its values
 */
private[event] class AttributesFromStartElement(start: NativeStartElem) extends Attributes {
	def get(qname: QName) = {
		val attr = start.getAttributeByName(qname)
		if (attr == null) {
			None
		} else {
			Some(attr.getValue)
		}
	}

	def iterator = {
		import collection.JavaConverters._
		start.getAttributes.asScala.map { attrObj =>
			val attr = attrObj.asInstanceOf[Attribute]
			attr.getName -> attr.getValue
		}
	}
}

/** Extractor for StartElement events, returning the QName and an
  * Attributes instance if the matched event was a StartElement.
  */
object StartElement {
	def unapply(event: XMLEvent): Option[(QName, Attributes)] = {
		if (event.isStartElement) {
			val start = event.asStartElement
			val name = start.getName
			val attribs = new AttributesFromStartElement(start)
			Some(name -> attribs)
		} else {
			None
		}
	}
}

/** Extracts the QName of an EndElement event */
object EndElement {
	def unapply(event: XMLEvent): Option[QName] = {
		if (event.isEndElement) {
			val end = event.asEndElement
			Some(end.getName)
		} else {
			None
		}
	}
}

/** Extracts the textual data from any Characters event */
object Characters {
	def unapply(event: XMLEvent): Option[String] = {
		if (event.isCharacters) {
			val chars = event.asCharacters
			Some(chars.getData)
		} else {
			None
		}
	}
}

/** Extracts the contents of a Comment event */
object Comment {
	def unapply(event: XMLEvent): Option[String] = {
		if (event.isInstanceOf[NativeComment]) {
			val comment = event.asInstanceOf[NativeComment]
			Some(comment.getText)
		} else {
			None
		}
	}
}

/** Matches any StartDocument event */
object StartDocument {
	def unapply(event: XMLEvent): Boolean = {
		event.isStartDocument
	}
}

/** Matches any EndDocument event */
object EndDocument {
	def unapply(event: XMLEvent): Boolean = {
		event.isEndDocument
	}
}
