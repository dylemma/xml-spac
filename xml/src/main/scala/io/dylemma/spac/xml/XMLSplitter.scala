package io.dylemma.spac.xml

import javax.xml.namespace.QName
import javax.xml.stream.events.{StartElement, XMLEvent}

import io.dylemma.spac.TransformerSyntax._
import io.dylemma.spac.xml.XMLParser.handlerFactoryConverter
import io.dylemma.spac.{BaseStackSplitter, ContextMatcher, HandlerFactory, SplitterApply}

import scala.util.Try

class XMLSplitter[+Context](matcher: ContextMatcher[StartElement, Context]) extends BaseStackSplitter(matcher) {
	splitterSelf =>

	def attr(name: QName) = through(XMLParser.forMandatoryAttribute(name))
	def attr(name: String) = through(XMLParser.forMandatoryAttribute(name))
	def asText = through(XMLParser.forText)

	def asListOf[Out](implicit parser: Context => HandlerFactory[XMLEvent, Out]) = as[Out].parseToList

	object first {
		def apply[Out](implicit parser: Context => HandlerFactory[XMLEvent, Out]) = through(parser).parseFirst
		def attr(name: QName) = through(XMLParser.forMandatoryAttribute(name)).parseFirst
		def attr(name: String) = through(XMLParser.forMandatoryAttribute(name)).parseFirst
		def asText = through(XMLParser.forText).parseFirst
	}

	object firstOption {
		def apply[Out](implicit parser: Context => HandlerFactory[XMLEvent, Try[Out]]) = through(parser).parseFirstOption
		def attr(name: QName) = through(XMLParser.forMandatoryAttribute(name)).parseFirstOption
		def attr(name: String) = through(XMLParser.forMandatoryAttribute(name)).parseFirstOption
		def asText = through(XMLParser.forText).parseFirstOption
	}

}

object XMLSplitter extends SplitterApply[StartElement, XMLSplitter] {
	companion =>
	def apply[Context](matcher: ContextMatcher[StartElement, Context]) = new XMLSplitter(matcher)
}