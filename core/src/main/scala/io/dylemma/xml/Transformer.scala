package io.dylemma.xml

import javax.xml.stream.events.XMLEvent
import scala.collection.generic.CanBuildFrom
import scala.concurrent.ExecutionContext

import io.dylemma.xml.Result._
import play.api.libs.iteratee.{ Enumeratee, Iteratee }

trait TransformerCommon[A] {
	type T[B]
	type P[B]

	def parseWith[B](getIteratee: ExecutionContext => Iteratee[Result[A], Result[B]]): P[B]
	def transformWith[B](getEnumeratee: ExecutionContext => Enumeratee[Result[A], Result[B]]): T[B]

	@inline def parseWith[B](iteratee: Iteratee[Result[A], Result[B]]): P[B] = parseWith(_ => iteratee)
	@inline def transformWith[B](enumeratee: Enumeratee[Result[A], Result[B]]): T[B] = transformWith(_ => enumeratee)

	@inline def parseSingle: P[A] = parseWith { implicit ec => Transformer.consumeSingle[A] }
	@inline def parseOptional: P[Option[A]] = parseWith { implicit ec => Transformer.consumeOptional[A] }
	@inline def parseList: P[List[A]] = parseWith { implicit ec => Transformer.consumeList[A] }
	@inline def parseConcat[B, That]()(implicit t: A => TraversableOnce[B], bf: CanBuildFrom[A, B, That]): P[That] = {
		parseWith { implicit ec => Transformer.consumeConcat }
	}
	@inline def foreach(thunk: A => Unit): P[Unit] = {
		parseWith { implicit ec => Transformer.runSideEffect(_ foreach thunk) }
	}
	@inline def foreachResult(thunk: Result[A] => Unit): P[Unit] = {
		parseWith { implicit ec => Transformer.runSideEffect(thunk) }
	}

	@inline def takeThroughNthError(n: Int): T[A] = transformWith { implicit ec => IterateeHelpers.takeThroughNthError(n) }
	@inline def takeThroughFirstError: T[A] = takeThroughNthError(1)
	@inline def takeUntilNthError(n: Int): T[A] = transformWith { implicit ec => IterateeHelpers.takeUntilNthError(n) }
	@inline def takeUntilFirstError: T[A] = takeUntilNthError(1)

	@inline def scanResultsWith[B](s: StreamScan[A, B]) = transformWith { implicit ec => IterateeHelpers.scanResultsWith(s) }
	@inline def scanWith[B](s: StreamScan[A, B]) = transformWith { implicit ec => IterateeHelpers.scanWith(s) }
}

trait Transformer[A] extends TransformerCommon[A] { self =>
	def toEnumeratee(implicit ec: ExecutionContext): Enumeratee[XMLEvent, Result[A]]

	type T[B] = Transformer[B]
	type P[B] = Parser[B]

	def parseWith[B](getIteratee: ExecutionContext => Iteratee[Result[A], Result[B]]): Parser[B] = {
		new Parser[B] {
			def toIteratee(implicit ec: ExecutionContext) = toEnumeratee transform getIteratee(ec)
		}
	}

	def transformWith[B](getEnumeratee: ExecutionContext => Enumeratee[Result[A], Result[B]]): Transformer[B] = new Transformer[B] {
		def toEnumeratee(implicit ec: ExecutionContext) = {
			self.toEnumeratee ><> getEnumeratee(ec)
		}
	}

}

trait TransformerForContext[In, A] extends TransformerCommon[A] { self =>
	def toEnumeratee(in: In)(implicit ec: ExecutionContext): Enumeratee[XMLEvent, Result[A]]

	type T[B] = TransformerForContext[In, B]
	type P[B] = ParserForContext[In, B]

	def parseWith[B](getIteratee: ExecutionContext => Iteratee[Result[A], Result[B]]): ParserForContext[In, B] = {
		new ParserForContext[In, B] {
			def toIteratee(context: In)(implicit ec: ExecutionContext) = toEnumeratee(context) transform getIteratee(ec)
		}
	}

	def transformWith[B](getEnumeratee: ExecutionContext => Enumeratee[Result[A], Result[B]]): TransformerForContext[In, B] = {
		new TransformerForContext[In, B] {
			def toEnumeratee(in: In)(implicit ec: ExecutionContext): Enumeratee[XMLEvent, Result[B]] = {
				self.toEnumeratee(in) ><> getEnumeratee(ec)
			}
		}
	}

}

object Transformer {

	implicit object TransformerMapR extends MapR[Transformer] {
		def mapR[A, B](ma: Transformer[A], f: Result[A] => Result[B]): Transformer[B] = new Transformer[B] {
			def toEnumeratee(implicit ec: ExecutionContext) = {
				ma.toEnumeratee ><> Enumeratee.map(f)
			}
		}
	}

	def consumeSingle[A](implicit ec: ExecutionContext): Iteratee[Result[A], Result[A]] = {
		Iteratee.head map { headOpt => headOpt getOrElse Empty }
	}

	def consumeOptional[A](implicit ec: ExecutionContext): Iteratee[Result[A], Result[Option[A]]] = {
		Iteratee.head map {
			case None => Success(None)
			case Some(Empty) => Success(None)
			case Some(headResult) => headResult.map(Some(_))
		}
	}

	def consumeList[A](implicit ec: ExecutionContext): Iteratee[Result[A], Result[List[A]]] = {
		Iteratee.getChunks map { chunks =>
			Result.list(chunks)
		}
	}

	def consumeConcat[A, B, That](
		implicit ec: ExecutionContext, t: A => TraversableOnce[B], bf: CanBuildFrom[A, B, That]
	): Iteratee[Result[A], Result[That]] = {
		consumeList[A] map { listResult =>
			listResult map { list =>
				val builder = bf()
				list foreach (builder ++= _)
				builder.result()
			}
		}
	}

	def runSideEffect[A](thunk: Result[A] => Unit)(implicit ec: ExecutionContext): Iteratee[Result[A], Result[Unit]] = {
		Iteratee.foreach(thunk) map Success.apply
	}
}

object TransformerForContext {
	implicit object TransformerForContextMapper extends MapRC[TransformerForContext] {
		override def mapR[X, A, B](m: TransformerForContext[X, A], f: (Result[A]) => Result[B]): TransformerForContext[X, B] = new TransformerForContext[X, B] {
			def toEnumeratee(in: X)(implicit ec: ExecutionContext) = {
				m.toEnumeratee(in) ><> Enumeratee.map(f)
			}
		}
	}
}