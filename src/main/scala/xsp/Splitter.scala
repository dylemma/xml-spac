package xsp

import javax.xml.stream.events.XMLEvent

import xsp.handlers.XMLContextSplitterHandler

trait Splitter[+Context] {
	def through[Out](parser: Parser[Context, Out]): Transformer[XMLEvent, Out]
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