package xsp

import javax.xml.stream.events.XMLEvent

import xsp.handlers.AbstractParser

trait Parser[-Context, +Out] {
	def makeHandler(context: Context): Handler[XMLEvent, Result[Out]]
	def makeHandler(contextError: Throwable): Handler[XMLEvent, Result[Out]]
}
object Parser {
	def fromConsumer[Out](consumer: Consumer[XMLEvent, Result[Out]]): Parser[Any, Out] = {
		new AbstractParser[Any, Out] {
			def makeHandler(context: Any) = consumer.makeHandler()
		}
	}
}

trait Consumer[In, Out] {
	def makeHandler(): Handler[In, Out]
}

trait Transformer[In, B] { self =>
	def makeHandler[Out](next: Handler[B, Out]): Handler[In, Out]

	def andThen[C](nextT: Transformer[B, C]): Transformer[In, C] = >>(nextT)
	def >>[C](nextT: Transformer[B, C]): Transformer[In, C] = new Transformer[In, C] {
		def makeHandler[Out](next: Handler[C, Out]): Handler[In, Out] = {
			self.makeHandler(nextT.makeHandler(next))
		}
	}

	def andThen[Out](end: Consumer[B, Out]): Consumer[In, Out] = >>(end)
	def >>[Out](end: Consumer[B, Out]): Consumer[In, Out] = new Consumer[In, Out] {
		def makeHandler(): Handler[In, Out] = {
			self.makeHandler(end.makeHandler())
		}
	}
}

trait Splitter[+Context] {
	def through[Out](parser: Parser[Context, Out]): Transformer[XMLEvent, Out]
}













