package xsp

import xsp.handlers._

object ExampleConsumers {
	case class Take[A](max: Int) extends Transformer[A, A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[A, Out] = {
			new TakeNHandler[A, Out](max, next)
		}
	}

	case class TakeWhile[A](p: A => Boolean) extends Transformer[A, A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[A, Out] = {
			new TakeWhileHandler[A, Out](p, next)
		}
	}

	case class Filter[A](p: A => Boolean) extends Transformer[A, A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[A, Out] = {
			new FilteringHandler[A, Out](p, next)
		}
	}

	case class ToList[A]() extends Consumer[A, Result[List[A]]] {
		def makeHandler(): Handler[A, Result[List[A]]] = {
			new ToListHandler[A]
		}
	}
}
