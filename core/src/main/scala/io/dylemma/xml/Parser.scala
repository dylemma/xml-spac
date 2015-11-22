package io.dylemma.xml

import javax.xml.stream.events.XMLEvent
import scala.concurrent.{ Future, ExecutionContext }

import play.api.libs.iteratee.{ Enumerator, Iteratee }

/**
 * Created by dylan on 11/5/2015.
 */

trait Parser[-C, +T] { self =>
	import Result._

	def toIteratee(context: C)(implicit ec: ExecutionContext): Iteratee[XMLEvent, Result[T]]

	def parse[In: AsInputStream](input: In)(implicit ec: ExecutionContext, ev: Unit <:< C): Future[Result[T]] = {
		parse(XMLEventEnumerator(input))
	}

	def parse(stream: Enumerator[XMLEvent])(implicit ec: ExecutionContext, ev: Unit <:< C): Future[Result[T]] = {
		val consumer = toIteratee(())
		stream run consumer
	}

	/** Creates a new Parser that passes successful results through a
		* transformation function (`f`)
		*/
	def map[U](f: T => U): Parser[C, U] = new Parser[C, U] {
		def toIteratee(context: C)(implicit ec: ExecutionContext) = self.toIteratee(context).map(_ map f)
	}

	/** Creates a new Parser that transforms each result according to
		* a transformation function (`f`).
		*/
	def mapR[U](f: Result[T] => Result[U]) = new Parser[C, U] {
		def toIteratee(context: C)(implicit ec: ExecutionContext) = self.toIteratee(context).map(f)
	}

	/** Create a parser that adapts to another context type (`C1`) by mapping
		* values from that type to an appropriate context value for this parser.
		*/
	def mapContext[C1](f: C1 => C) = new Parser[C1, T] {
		def toIteratee(context: C1)(implicit ec: ExecutionContext) = self.toIteratee(f(context))
	}
}
