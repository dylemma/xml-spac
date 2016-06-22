package io.dylemma.xsp

import javax.xml.namespace.QName
import javax.xml.stream.events.XMLEvent

import io.dylemma.xsp.handlers.XMLContextSplitterHandler
import TransformerSyntax._

trait Splitter[+Context] {
	def through[Out](parser: Parser[Context, Out]): Transformer[XMLEvent, Out]

	def as[Out](implicit parser: Parser[Context, Out]) = through(parser)
	def attr(name: QName) = through(Parser.forMandatoryAttribute(name))
	def attr(name: String) = through(Parser.forMandatoryAttribute(name))
	def asText = through(Parser.forText)

	def asListOf[Out](implicit parser: Parser[Context, Out]) = as[Out].parseToList

	object first {
		def apply[Out](implicit parser: Parser[Context, Out]) = through(parser).parseFirst
		def attr(name: QName) = through(Parser.forMandatoryAttribute(name)).parseFirst
		def attr(name: String) = through(Parser.forMandatoryAttribute(name)).parseFirst
		def asText = through(Parser.forText).parseFirst
	}

	object firstOption {
		def apply[Out](implicit parser: Parser[Context, Out]) = through(parser).parseFirstOption
		def attr(name: QName) = through(Parser.forMandatoryAttribute(name)).parseFirstOption
		def attr(name: String) = through(Parser.forMandatoryAttribute(name)).parseFirstOption
		def asText = through(Parser.forText).parseFirstOption
	}

}

object Splitter {

	def apply[Context](matcher: ContextMatcher[Context]): Splitter[Context] = new Splitter[Context] { self =>
		def through[P](parser: Parser[Context, P]): Transformer[XMLEvent, P] = {
			new Transformer[XMLEvent, P] {
				def makeHandler[Out](next: Handler[P, Out]): Handler[XMLEvent, Out] = {
					new XMLContextSplitterHandler(matcher, parser, next)
				}
				override def toString = s"$self{ $parser }"
			}
		}
		override def toString = s"Splitter($matcher)"
	}
}