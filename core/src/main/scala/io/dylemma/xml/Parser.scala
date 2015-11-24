package io.dylemma.xml

import javax.xml.stream.XMLStreamException
import javax.xml.stream.events.XMLEvent
import scala.concurrent.{ Future, ExecutionContext }
import language.higherKinds
import scala.util.control.NonFatal

import io.dylemma.xml.Result._
import io.dylemma.xml.event.{ Characters, StartElement }
import play.api.libs.iteratee.{ Done, Enumeratee, Enumerator, Iteratee }


/**
 * Created by dylan on 11/5/2015.
 */
trait ParserBase[-In, +Out] { self =>
	def toIteratee(context: In)(implicit ec: ExecutionContext): Iteratee[XMLEvent, Result[Out]]

	def unmapContext[In2](f: In2 => In): ParserBase[In2, Out] = new ParserBase[In2, Out]{
		def toIteratee(context: In2)(implicit ec: ExecutionContext) = {
			try{ self toIteratee f(context) }
			catch { case NonFatal(err) => Iteratee.skipToEof.map(_ => Error(err)) }
		}
	}
}

trait ParserForContext[-C, +T] extends ParserBase[C, T] with MapRC[C, T, ParserForContext]{ self =>


	def inContext(context: C): Parser[T] = new Parser[T] {
		def toIteratee(implicit ec: ExecutionContext) = self.toIteratee(context)
	}

	/** Creates a new Parser that transforms each result according to
		* a transformation function (`f`).
		*/
	@inline def mapR[U](f: Result[T] => Result[U]) = new ParserForContext[C, U] {
		def toIteratee(context: C)(implicit ec: ExecutionContext) = self.toIteratee(context).map(f)
	}

	/*
	 * Note that `def &` for ParserForContext are implemented separately,
	 * in the `ParserCombinerOps.ParserWithCombine` implicit class. This is done
	 * to avoid confusing the compiler with the contravariant context type.
	 */

	/** Create a parser that adapts to another context type (`C1`) by mapping
		* values from that type to an appropriate context value for this parser.
		*/
	def mapContext[C1](f: C1 => C) = new ParserForContext[C1, T] {
		def toIteratee(context: C1)(implicit ec: ExecutionContext) = self.toIteratee(f(context))
	}

}

trait Parser[+T] extends ParserBase[Any, T] with MapR[T, Parser] { self =>
	def toIteratee(implicit ec: ExecutionContext): Iteratee[XMLEvent, Result[T]]

	def toIteratee(context: Any)(implicit ec: ExecutionContext) = toIteratee

	def parse[In: AsInputStream](input: In)(implicit ec: ExecutionContext): Future[Result[T]] = {
		parse(XMLEventEnumerator(input))
	}

	def parse(stream: Enumerator[XMLEvent])(implicit ec: ExecutionContext): Future[Result[T]] = {
		val consumer = toIteratee
		stream run consumer
	}

	@inline def mapR[U](f: Result[T] => Result[U]): Parser[U] = new Parser[U] {
		def toIteratee(implicit ec: ExecutionContext) = self.toIteratee.map(f)
	}

	def &[U](parser2: Parser[U]): Parser[Chain[T, U]] = new Parser[Chain[T, U]] {
		def toIteratee(implicit ec: ExecutionContext) = {
			Enumeratee.zipWith(self.toIteratee, parser2.toIteratee){ (rT, rU) =>
				for(t <- rT; u <- rU) yield Chain(t, u)
			}
		}
	}

	def &[C, U](parser2: ParserForContext[C, U]): ParserForContext[C, Chain[T, U]] = new ParserForContext[C, Chain[T, U]] {
		def toIteratee(context: C)(implicit ec: ExecutionContext) = {
			Enumeratee.zipWith(self.toIteratee, parser2.toIteratee(context)){ (rT, rU) =>
				for(t <- rT; u <- rU) yield Chain(t, u)
			}
		}
	}
}

object Parser {

	def fromIteratee[A](f: ExecutionContext => Iteratee[XMLEvent, Result[A]]): Parser[A] = {
		new Parser[A] {
			def toIteratee(implicit ec: ExecutionContext) = f(ec)
		}
	}

	def parseOptionalAttribute(attribute: String) = fromIteratee { implicit ec =>
		val lookupAttr = Enumeratee.collect[XMLEvent] {
			case StartElement(_, attrs) => Success(attrs get attribute)
		}
		lookupAttr &>> Iteratee.head.map {
			// if the *head* was None, it means we never even encountered an element, so give an `Empty` result
			case None => Empty
			// otherwise, some element was encountered, and the result is whatever optional attribute value was there
			case Some(result) => result
		}
	}

	def parseMandatoryAttribute(attribute: String) = fromIteratee { implicit ec =>
		val lookupAttr = Enumeratee.collect[XMLEvent]{
			case e @ StartElement(_, attrs) => attrs.get(attribute) match {
				case None =>
					val msg = s"Expected a value for the '$attribute' attribute, but none was found"
					Error(new XMLStreamException(msg, e.getLocation))
				case Some(value) =>
					Success(value)
			}
		}
		// if no elements were found, return Empty rather than an error
		lookupAttr &>> Iteratee.head.map { _ getOrElse Empty }
	}

	val parseText = fromIteratee { implicit ec =>
		val collectText = Enumeratee.collect[XMLEvent] { case Characters(text) => text.trim }
		val consumeTextAsSuccess = Iteratee.consume[String]().map[Result[String]](Success(_))
		collectText &>> consumeTextAsSuccess
	}

}