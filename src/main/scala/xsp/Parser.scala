package xsp

import javax.xml.namespace.QName
import javax.xml.stream.events.XMLEvent

import xsp.handlers._

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

	// TEXT
	def forText: Parser[Any, String] = ForText
	object ForText extends AbstractParser[Any, String] {
		def makeHandler(context: Any) = new TextCollectorHandler
	}

	// CONTEXT
	def forContext[C]: Parser[C, C] = new ForContext[C]
	class ForContext[C] extends AbstractParser[C, C] {
		def makeHandler(context: C) = new OneShotHandler(Result.Success(context))
	}

	// ATTRIBUTE
	def forMandatoryAttribute(name: QName): Parser[Any, String] = new ForMandatoryAttribute(name)
	def forMandatoryAttribute(name: String): Parser[Any, String] = new ForMandatoryAttribute(new QName(name))
	class ForMandatoryAttribute(name: QName) extends AbstractParser[Any, String] {
		def makeHandler(context: Any) = new MandatoryAttributeHandler(name)
	}

	// OPTIONAL ATTRIBUTE
	def forOptionalAttribute(name: QName): Parser[Any, Option[String]] = new ForOptionalAttribute(name)
	def forOptionalAttribute(name: String): Parser[Any, Option[String]] = new ForOptionalAttribute(new QName(name))
	class ForOptionalAttribute(name: QName) extends AbstractParser[Any, Option[String]] {
		def makeHandler(context: Any) = new OptionalAttributeHandler(name)
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















