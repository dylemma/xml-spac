package xsp

import xsp.handlers._

/** An immutable object that can be used to create `Handler`s.
	*/
trait Consumer[-In, +Out] {
	def makeHandler(): Handler[In, Out]
}

object Consumer {

	case class ToList[A]() extends Consumer[A, Result[List[A]]] {
		def makeHandler(): Handler[A, Result[List[A]]] = {
			new ToListHandler[A]
		}
	}

	case class First[A]() extends Consumer[A, Result[A]] {
		def makeHandler(): Handler[A, Result[A]] = {
			new GetFirstHandler[A]
		}
	}

	case class FirstOption[A]() extends Consumer[A, Result[Option[A]]] {
		def makeHandler(): Handler[A, Result[Option[A]]] = {
			new GetFirstOptionHandler[A]
		}
	}

	case class Fold[A, R](init: R, f: (R, A) => R) extends Consumer[A, Result[R]] {
		def makeHandler(): Handler[A, Result[R]] = {
			new FoldHandler(init, f)
		}
	}

	case class FoldResults[A, R](
		init: Result[R],
		f: (Result[R], Result[A]) => Result[R]
	) extends Consumer[A, Result[R]] {
		def makeHandler(): Handler[A, Result[R]] = {
			new FoldResultsHandler(init, f)
		}
	}
}