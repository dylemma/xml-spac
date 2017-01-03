package io.dylemma.spac

import javax.xml.namespace.QName
import javax.xml.stream.events.XMLEvent

import io.dylemma.spac.handlers._

import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

trait Parser[+Out] { self =>
	def makeHandler(): Handler[XMLEvent, Try[Out]]

	def mapResult[B](f: Try[Out] => Try[B]): Parser[B] = new Parser[B] {
		def makeHandler() = new MappedConsumerHandler(f, self.makeHandler())
		override def toString = s"$self >> Map($f)"
	}

	def map[B](f: Out => B): Parser[B] = mapResult(_ map f)
	def flatMap[B](f: Out => Try[B]): Parser[B] = mapResult(_ flatMap f)

	def and[O2](p2: Parser[O2]) = new ParserCombination.Combined2(self, p2)
	def ~[O2](p2: Parser[O2]) = new ParserCombination.Combined2(self, p2)

	/** If a `Parser` is context-independent, it can be treated to a `Consumer`.
		*
		* @return A representation of this parser as a `Consumer`
		*/
	def toConsumer: Consumer[XMLEvent, Try[Out]] = {
		new Consumer[XMLEvent, Try[Out]] {
			def makeHandler(): Handler[XMLEvent, Try[Out]] = self.makeHandler()
			override def toString = self.toString
		}
	}

	def parse[XML](xml: XML)(
		implicit consumeXML: ConsumableLike[XML, XMLEvent]
	): Try[Out] = consumeXML(xml, makeHandler())
}

object Parser {
	def fromConsumer[Out](consumer: Consumer[XMLEvent, Try[Out]]): Parser[Out] = {
		new Parser[Out] {
			def makeHandler() = consumer.makeHandler()
			override def toString = consumer.toString
		}
	}

	// TEXT
	def forText: Parser[String] = ForText
	object ForText extends Parser[String] {
		def makeHandler() = new TextCollectorHandler
		override def toString = "XMLText"
	}

	// ATTRIBUTE
	def forMandatoryAttribute(name: QName): Parser[String] = new ForMandatoryAttribute(name)
	def forMandatoryAttribute(name: String): Parser[String] = new ForMandatoryAttribute(new QName(name))
	class ForMandatoryAttribute(name: QName) extends Parser[String] {
		def makeHandler() = new MandatoryAttributeHandler(name)
		override def toString = s"Attribute($name)"
	}

	// OPTIONAL ATTRIBUTE
	def forOptionalAttribute(name: QName): Parser[Option[String]] = new ForOptionalAttribute(name)
	def forOptionalAttribute(name: String): Parser[Option[String]] = new ForOptionalAttribute(new QName(name))
	class ForOptionalAttribute(name: QName) extends Parser[Option[String]] {
		def makeHandler() = new OptionalAttributeHandler(name)
		override def toString = s"OptionalAttribute($name)"
	}

	// CONSTANT
	def constant[A](result: A): Parser[A] = new Parser[A] {
		def makeHandler(): Handler[XMLEvent, Try[A]] = {
			new SafeConsumerHandler(new ConstantHandler(result))
		}
		override def toString = s"Constant($result)"
	}
}



















