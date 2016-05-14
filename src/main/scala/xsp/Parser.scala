package xsp

import javax.xml.stream.events.XMLEvent

trait Parser[-Context, +Out] {
	def makeHandler(context: Context): Handler[XMLEvent, Out]
}

trait Consumer[In, Out] {
	def makeHandler(): Handler[In, Out]
}

trait Transformer[In, B] { self =>
	def makeHandler[Out](next: Handler[B, Out]): Handler[In, Out]

	def >>[C](nextT: Transformer[B, C]): Transformer[In, C] = new Transformer[In, C] {
		def makeHandler[Out](next: Handler[C, Out]): Handler[In, Out] = {
			self.makeHandler(nextT.makeHandler(next))
		}
	}

	def >>[Out](end: Consumer[B, Out]): Consumer[In, Out] = new Consumer[In, Out] {
		def makeHandler(): Handler[In, Out] = {
			self.makeHandler(end.makeHandler())
		}
	}
}

trait Splitter[+Context] {
	def through[Out](parser: Parser[Context, Out]): Transformer[XMLEvent, Out]
}

trait Handler[-In, +Out] {
	def handleInput(input: In): Option[Out]
	def handleError(err: Throwable): Option[Out]
	def handleEnd(): Out
	def isFinished: Boolean
}











