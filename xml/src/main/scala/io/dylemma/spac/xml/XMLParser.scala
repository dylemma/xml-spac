package io.dylemma.spac.xml

import javax.xml.namespace.QName
import javax.xml.stream.events.XMLEvent

import io.dylemma.spac.xml.handlers.{MandatoryAttributeHandler, OptionalAttributeHandler, TextCollectorHandler}
import io.dylemma.spac.{FromHandlerFactory, HandlerFactory, ParserCompanion, ParserLike}

abstract class XMLParser[+A] extends ParserLike[XMLEvent, A, XMLParser]

object XMLParser extends ParserCompanion[XMLEvent, XMLParser] {

	implicit val handlerFactoryConverter: FromHandlerFactory[XMLEvent, XMLParser] = new FromHandlerFactory[XMLEvent, XMLParser] {
		override def makeInstance[Out](hf: HandlerFactory[XMLEvent, Out], debugName: String): XMLParser[Out] = new XMLParser[Out] {
		def makeHandler() = hf.makeHandler()
		override def toString = debugName
	}
}

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
