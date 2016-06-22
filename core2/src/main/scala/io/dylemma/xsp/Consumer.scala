package io.dylemma.xsp

import io.dylemma.xsp.handlers._

/** An immutable object that can be used to create `Handler`s.
	*/
trait Consumer[-In, +Out] {
	def makeHandler(): Handler[In, Out]

	def consume[S](source: S)(implicit consume: ConsumableLike[S, In]): Out = {
		consume(source, makeHandler())
	}
}

object Consumer {

	case class ToList[A]() extends Consumer[A, Result[List[A]]] {
		def makeHandler(): Handler[A, Result[List[A]]] = {
			new ToListHandler[A]
		}
		override def toString = "ToList"
	}

	case class First[A]() extends Consumer[A, Result[A]] {
		def makeHandler(): Handler[A, Result[A]] = {
			new GetFirstHandler[A]
		}
		override def toString = "First"
	}

	case class FirstOption[A]() extends Consumer[A, Result[Option[A]]] {
		def makeHandler(): Handler[A, Result[Option[A]]] = {
			new GetFirstOptionHandler[A]
		}
		override def toString = "FirstOption"
	}

	case class Fold[A, R](init: R, f: (R, A) => R) extends Consumer[A, Result[R]] {
		def makeHandler(): Handler[A, Result[R]] = {
			new FoldHandler(init, f)
		}
		override def toString = s"Fold($init, $f)"
	}

	case class FoldResults[A, R](
		init: Result[R],
		f: (Result[R], Result[A]) => Result[R]
	) extends Consumer[A, Result[R]] {
		def makeHandler(): Handler[A, Result[R]] = {
			new FoldResultsHandler(init, f)
		}
		override def toString = s"FoldResults($init, $f)"
	}

	case class ForEach[A](f: A => Any) extends Consumer[A, Unit] {
		def makeHandler() = new ForEachHandler(f)
		override def toString = s"ForEach($f)"
	}
}