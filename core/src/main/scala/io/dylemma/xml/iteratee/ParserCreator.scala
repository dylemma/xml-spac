package io.dylemma.xml.iteratee

import javax.xml.stream.events.XMLEvent
import scala.collection.generic.CanBuildFrom
import scala.concurrent.ExecutionContext

import play.api.libs.iteratee.{ Enumeratee, Iteratee }

trait ParserCreator[T] {
	import Parser._

	def toEnumeratee(implicit ec: ExecutionContext): Enumeratee[XMLEvent, Result[T]]

	// create a parser from a 'consumer' iteratee
	def asParser[M](consumer: Iteratee[Result[T], Result[M]]): Parser[M] = new Parser[M] {
		def toIteratee(implicit ec: ExecutionContext) = toEnumeratee &>> consumer
	}
	// create a parser from a 'consumer' iteratee that is constructed with an ExecutionContext available
	def asParser[M](makeConsumer: ExecutionContext => Iteratee[Result[T], Result[M]]): Parser[M] = new Parser[M] {
		def toIteratee(implicit ec: ExecutionContext) = toEnumeratee &>> makeConsumer(ec)
	}

	// creates a parser that gets the first result, or else an Empty
	def parseSingle: Parser[T] = asParser { implicit ec =>
		Iteratee.head.map { headOpt => headOpt getOrElse Empty }
	}

	// creates a parser that gets the optional first result
	def parseOptional: Parser[Option[T]] = asParser { implicit ec =>
		Iteratee.head.map {
			case None => Success(None)
			case Some(Empty) => Success(None)
			case Some(headResult) => headResult.map(Some(_))
		}
	}

	// creates a parser that gathers all of the results in a list
	// (returns an Error if any of the results were an error)
	def parseList: Parser[List[T]] = asParser { implicit ec =>
		Iteratee.getChunks.map { chunks =>
			chunks.foldRight[Result[List[T]]](Success(Nil)) { (next, accum) =>
				next match {
					case Empty => accum
					case e: Error => e
					case Success(value) => accum.map {value :: _}
				}
			}
		}
	}

	def parseConsume[B, That]()(implicit t: T => TraversableOnce[B], bf: CanBuildFrom[T, B, That]): Parser[That] = {
		parseList.map { list =>
			val builder = bf()
			list.foreach(builder ++= _)
			builder.result()
		}
	}

	// creates a parser that passes results to the consumer which executes a side-effect
	def parseSideEffect(consumer: Iteratee[Result[T], Unit]): Parser[Unit] = asParser { implicit ec =>
		consumer.map{ Success(_) }
	}
}