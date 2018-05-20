package io.dylemma.spac.xml

import javax.xml.namespace.QName
import javax.xml.stream.events.{StartElement, XMLEvent}

import io.dylemma.spac.xml.handlers.XMLContextSplitterHandler
import io.dylemma.spac.{ContextMatcher, Handler, HandlerFactory, Splitter, SplitterApply, Transformer}

import scala.util.Try

trait XMLSplitter[+Context] extends Splitter[XMLEvent, Context] { splitterSelf =>

	def as[Out](implicit parser: Context => HandlerFactory[XMLEvent, Out]) = through(parser)
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

object XMLSplitter {
	implicit val xmlSplitterApply: SplitterApply[StartElement, XMLSplitter] = new SplitterApply[StartElement, XMLSplitter] {
		def apply[Context](matcher: ContextMatcher[StartElement, Context]): XMLSplitter[Context] = new XMLSplitter[Context] { self =>
			def through[P](joiner: Context => HandlerFactory[XMLEvent, P]): Transformer[XMLEvent, P] = {
				new Transformer[XMLEvent, P] {
					def makeHandler[Out](next: Handler[P, Out]): Handler[XMLEvent, Out] = {
						new XMLContextSplitterHandler(matcher, joiner, next)
					}
					override def toString = s"$self{ $joiner }"
				}
			}
			override def toString = s"Splitter($matcher)"
		}
	}
}