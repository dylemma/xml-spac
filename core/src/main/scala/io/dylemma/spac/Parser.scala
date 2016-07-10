package io.dylemma.spac

import javax.xml.namespace.QName
import javax.xml.stream.events.XMLEvent

import io.dylemma.spac.handlers._

import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

trait Parser[-Context, +Out] { self =>
	def makeHandler(context: Context): Handler[XMLEvent, Try[Out]]

	def mapResult[B](f: Try[Out] => Try[B]): Parser[Context, B] = new Parser[Context, B] {
		def makeHandler(context: Context) = new MappedConsumerHandler(f, self.makeHandler(context))
		override def toString = s"$self >> Map($f)"
	}

	def map[B](f: Out => B): Parser[Context, B] = mapResult(_ map f)
	def flatMap[B](f: Out => Try[B]): Parser[Context, B] = mapResult(_ flatMap f)

	/** Bind this `Parser` to a specific `context`.
		* The resulting parser ignores all context information passed to it for
		* purposes of creating a handler; it instead uses the context passed to
		* this method.
		*
		* @param context The `Context` value that will be used to create handlers
		*/
	def inContext(context: Context): Parser[Any, Out] = new Parser[Any, Out] {
		def makeHandler(ignored: Any) = self.makeHandler(context)
		override def toString = s"$self.inContext($context)"
	}

	/** If a `Parser` is context-independent, it can be treated to a `Consumer`.
		*
		* @param ev Implicit evidence that the parser's `Context` type is `Any`
		* @return A representation of this parser as a `Consumer`
		*/
	def toConsumer(implicit ev: Any <:< Context): Consumer[XMLEvent, Try[Out]] = {
		new Consumer[XMLEvent, Try[Out]] {
			def makeHandler(): Handler[XMLEvent, Try[Out]] = self.makeHandler(ev(()))
			override def toString = self.toString
		}
	}

	def parse[XML](xml: XML)(
		implicit consumeXML: ConsumableLike[XML, XMLEvent],
		anyContext: Any <:< Context
	): Try[Out] = consumeXML(xml, makeHandler(anyContext(())))
}
object Parser extends ParserCombineMethods {
	def fromConsumer[Out](consumer: Consumer[XMLEvent, Try[Out]]): Parser[Any, Out] = {
		new Parser[Any, Out] {
			def makeHandler(context: Any) = consumer.makeHandler()
			override def toString = consumer.toString
		}
	}

	// TEXT
	def forText: Parser[Any, String] = ForText
	object ForText extends Parser[Any, String] {
		def makeHandler(context: Any) = new TextCollectorHandler
		override def toString = "XMLText"
	}

	// CONTEXT
	def forContext[C]: Parser[C, C] = new ForContext[C]
	class ForContext[C] extends Parser[C, C] {
		def makeHandler(context: C) = new OneShotHandler(Success(context))
		override def toString = "XMLContext"
	}

	// ATTRIBUTE
	def forMandatoryAttribute(name: QName): Parser[Any, String] = new ForMandatoryAttribute(name)
	def forMandatoryAttribute(name: String): Parser[Any, String] = new ForMandatoryAttribute(new QName(name))
	class ForMandatoryAttribute(name: QName) extends Parser[Any, String] {
		def makeHandler(context: Any) = new MandatoryAttributeHandler(name)
		override def toString = s"Attribute($name)"
	}

	// OPTIONAL ATTRIBUTE
	def forOptionalAttribute(name: QName): Parser[Any, Option[String]] = new ForOptionalAttribute(name)
	def forOptionalAttribute(name: String): Parser[Any, Option[String]] = new ForOptionalAttribute(new QName(name))
	class ForOptionalAttribute(name: QName) extends Parser[Any, Option[String]] {
		def makeHandler(context: Any) = new OptionalAttributeHandler(name)
		override def toString = s"OptionalAttribute($name)"
	}

	// CHOOSE
	def choose[Context] = new ChooseApply[Context]
	class ChooseApply[Context] {
		def apply[Out](chooser: Context => Parser[Context, Out]): Parser[Context, Out] = {
			new Parser[Context, Out] {
				def makeHandler(context: Context): Handler[XMLEvent, Try[Out]] = {
					try chooser(context).makeHandler(context)
					catch {
						case NonFatal(err) =>
							val wrapped = new Exception(s"Failed to choose a parser for context [$context]", err)
							new OneShotHandler(Failure(wrapped))
					}
				}
				override def toString = s"Choose($chooser)"
			}
		}
	}

}



















