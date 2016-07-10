package io.dylemma.spac

import io.dylemma.spac.handlers._

import scala.util.Try

/** An immutable object that can be used to create `Handler`s.
	*/
trait Consumer[-In, +Out] { self =>
	def makeHandler(): Handler[In, Out]

	def consume[S](source: S)(implicit consume: ConsumableLike[S, In]): Out = {
		consume(source, makeHandler())
	}

	def map[U](f: Out => U): Consumer[In, U] = new Consumer[In, U] {
		def makeHandler(): Handler[In, U] = new MappedConsumerHandler(f, self.makeHandler())
	}

	def wrapSafe: Consumer[In, Try[Out]] = new Consumer[In, Try[Out]] {
		def makeHandler(): Handler[In, Try[Out]] = new SafeConsumerHandler(self.makeHandler())
	}
}

object Consumer {

	case class ToList[A]() extends Consumer[A, List[A]] {
		def makeHandler(): Handler[A, List[A]] = {
			new ToListHandler[A]
		}
		override def toString = "ToList"
	}

	case class First[A]() extends Consumer[A, A] {
		def makeHandler(): Handler[A, A] = {
			new GetFirstHandler[A]
		}
		override def toString = "First"
	}

	case class FirstOption[A]() extends Consumer[A, Option[A]] {
		def makeHandler(): Handler[A, Option[A]] = {
			new GetFirstOptionHandler[A]
		}
		override def toString = "FirstOption"
	}

	case class Fold[A, R](init: R, f: (R, A) => R) extends Consumer[A, R] {
		def makeHandler(): Handler[A, R] = {
			new FoldHandler(init, f)
		}
		override def toString = s"Fold($init, $f)"
	}

	case class ForEach[A](f: A => Any) extends Consumer[A, Unit] {
		def makeHandler() = new ForEachHandler(f)
		override def toString = s"ForEach($f)"
	}
}