package io.dylemma.spac

import javax.xml.namespace.QName
import javax.xml.stream.events.XMLEvent

import io.dylemma.spac.handlers.{SplitOnMatchHandler, XMLContextSplitterHandler}

import scala.util.Try

trait Splitter[In, +Context] {
	def through[Out](parser: Context => Handler[In, Try[Out]]): Transformer[In, Out]

	def through[Out](consumer: Consumer[In, Out]): Transformer[In, Out] = {
		val safeConsumer = consumer.wrapSafe
		through{ _ => safeConsumer.makeHandler() }
	}
}

trait XmlSplitter[+Context] extends Splitter[XMLEvent, Context] {

	def through[Out](parser: Parser[Context, Out]): Transformer[XMLEvent, Out] = {
		through(parser makeHandler _)
	}

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

	def apply[Context](matcher: ContextMatcher[Context]): XmlSplitter[Context] = new XmlSplitter[Context] { self =>
		def through[P](parser: (Context) => Handler[XMLEvent, Try[P]]): Transformer[XMLEvent, P] = {
			new Transformer[XMLEvent, P] {
				def makeHandler[Out](next: Handler[P, Out]): Handler[XMLEvent, Out] = {
					new XMLContextSplitterHandler(matcher, parser, next)
				}
				override def toString = s"$self{ $parser }"
			}
		}
		override def toString = s"Splitter($matcher)"
	}

	def splitOnMatch[In, Context](matcher: PartialFunction[In, Context]): Splitter[In, Context] = {
		new Splitter[In, Context] {self =>
			override def toString = s"Splitter.splitOnMatch($matcher)"
			def through[Out](parser: (Context) => Handler[In, Try[Out]]): Transformer[In, Out] = {
				new Transformer[In, Out] {
					override def toString = s"$self{ $parser }"
					def makeHandler[A](downstream: Handler[Out, A]): Handler[In, A] = {
						new SplitOnMatchHandler(matcher, parser, downstream)
					}
				}
			}
		}
	}

	def splitOnMatch[In](p: In => Boolean): Splitter[In, Any] = splitOnMatch[In, Any] {
		case in if p(in) => ()
	}
}