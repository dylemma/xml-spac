package io.dylemma.xml.iteratee

import javax.xml.stream.events.XMLEvent
import scala.concurrent.ExecutionContext

import play.api.libs.iteratee.Iteratee

trait Parser[T] { self =>
	def toIteratee(implicit ec: ExecutionContext): Iteratee[XMLEvent, ParserResult[T]]

	/** Creates a new Parser that passes successful results through a
		* transformation function (`f`)
		*/
	def map[U](f: T => U): Parser[U] = new Parser[U] {
		def toIteratee(implicit ec: ExecutionContext) = self.toIteratee.map(_ map f)
	}

	/** Creates a new Parser that transforms each result according to
		* a transformation function (`f`).
		*/
	def mapR[U](f: ParserResult[T] => ParserResult[U]) = new Parser[U] {
		def toIteratee(implicit ec: ExecutionContext) = self.toIteratee.map(f)
	}
}