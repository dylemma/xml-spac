package io.dylemma.spac.old.xml

import io.dylemma.spac.old.xml.handlers._
import javax.xml.namespace.QName

object XMLParser {

	// TEXT
	val forText: XMLParser[String] = new XMLParser[String] {
		def makeHandler() = new TextCollectorHandler
		override def toString = "XMLText"
	}

	// ATTRIBUTE
	def forMandatoryAttribute(name: String): XMLParser[String] = forMandatoryAttribute(new QName(name))
	def forMandatoryAttribute(name: QName): XMLParser[String] = new XMLParser[String] {
		def makeHandler() = new MandatoryAttributeHandler(name)
		override def toString = s"Attribute($name)"
	}

	// OPTIONAL ATTRIBUTE
	def forOptionalAttribute(name: String): XMLParser[Option[String]] = forOptionalAttribute(new QName(name))
	def forOptionalAttribute(name: QName): XMLParser[Option[String]] = new XMLParser[Option[String]] {
		def makeHandler() = new OptionalAttributeHandler(name)
		override def toString = s"OptionalAttribute($name)"
	}

}
