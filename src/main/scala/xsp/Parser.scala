package xsp

import javax.xml.namespace.QName
import javax.xml.stream.events.XMLEvent

import xsp.handlers._

import scala.util.control.NonFatal

trait Parser[-Context, +Out] { self =>
	def makeHandler(context: Context): Handler[XMLEvent, Result[Out]]
	def makeHandler(contextError: Throwable): Handler[XMLEvent, Result[Out]]

	def mapResult[B](f: Result[Out] => Result[B]): Parser[Context, B] = new Parser[Context, B] {
		def makeHandler(context: Context) = new MappedHandler(self.makeHandler(context), f)
		def makeHandler(contextError: Throwable) = new MappedHandler(self.makeHandler(contextError), f)
	}

	def map[B](f: Out => B): Parser[Context, B] = mapResult(_ map f)

	/** Bind this `Parser` to a specific `context`.
		* The resulting parser ignores all context information passed to it for
		* purposes of creating a handler; it instead uses the context passed to
		* this method.
		* @param context The `Context` value that will be used to create handlers
		*/
	def inContext(context: Context): Parser[Any, Out] = new Parser[Any, Out] {
		def makeHandler(ignored: Any) = self.makeHandler(context)
		def makeHandler(contextError: Throwable) = self.makeHandler(contextError)
	}

	/** If a `Parser` is context-independent, it can be treated to a `Consumer`.
		*
		* @param ev Implicit evidence that the parser's `Context` type is `Any`
		* @return A representation of this parser as a `Consumer`
		*/
	def toConsumer(implicit ev: Any <:< Context): Consumer[XMLEvent, Result[Out]] = {
		new Consumer[XMLEvent, Result[Out]] {
			def makeHandler(): Handler[XMLEvent, Result[Out]] = self.makeHandler(ev(()))
		}
	}
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

	// CHOOSE
	def choose[Context] = new ChooseApply[Context]
	class ChooseApply[Context] {
		def apply[Out](chooser: Context => Parser[Context, Out]): Parser[Context, Out] = {
			new AbstractParser[Context, Out] {
				def makeHandler(context: Context): Handler[XMLEvent, Result[Out]] = {
					try chooser(context).makeHandler(context)
					catch { case NonFatal(err) => makeHandler(err) }
				}
			}
		}
	}
}



















