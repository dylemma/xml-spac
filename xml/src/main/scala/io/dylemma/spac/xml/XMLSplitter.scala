package io.dylemma.spac.xml

import javax.xml.namespace.QName
import javax.xml.stream.events.{StartElement, XMLEvent}

import io.dylemma.spac._
import XMLParser.handlerFactoryConverter

import scala.util.Try

class XMLSplitter[+Context](matcher: ContextMatcher[StartElement, Context]) extends ContextStackSplitter(matcher) { self =>

	def attr(name: QName): Transformer[XMLEvent, String] = map(XMLParser.forMandatoryAttribute(name))
	def attr(name: String): Transformer[XMLEvent, String] = map(XMLParser.forMandatoryAttribute(name))
	def asText: Transformer[XMLEvent, String] = map(XMLParser.forText)

	def asListOf[Out](implicit parser: Context => HandlerFactory[XMLEvent, Out]): XMLParser[List[Out]] = as[Out].parseToList

	object first {
		def apply[Out](implicit parser: Context => HandlerFactory[XMLEvent, Out]) = map(parser).parseFirst
		def attr(name: QName) = map(XMLParser.forMandatoryAttribute(name)).parseFirst
		def attr(name: String) = map(XMLParser.forMandatoryAttribute(name)).parseFirst
		def asText = map(XMLParser.forText).parseFirst
	}

	object firstOption {
		def apply[Out](implicit parser: Context => HandlerFactory[XMLEvent, Try[Out]]) = map(parser).parseFirstOption
		def attr(name: QName) = map(XMLParser.forMandatoryAttribute(name)).parseFirstOption
		def attr(name: String) = map(XMLParser.forMandatoryAttribute(name)).parseFirstOption
		def asText = self.map(XMLParser.forText).parseFirstOption
	}

}

object XMLSplitter extends SplitterApply[StartElement, XMLSplitter] {
	def apply[Context](matcher: ContextMatcher[StartElement, Context]) = new XMLSplitter(matcher)
}