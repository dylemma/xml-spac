package io.dylemma.spac

import io.dylemma.spac.handlers._

import scala.language.higherKinds
import scala.util.Try

/** An immutable object that can be used to create `Handler`s.
	*/
trait Consumer[-In, +Out] extends HandlerFactory[In, Out] { self =>

	def consume[S](source: S)(implicit consume: ConsumableLike[S, In]): Out = {
		consume(source, makeHandler())
	}

	def map[U](f: Out => U): Consumer[In, U] = new Consumer[In, U] {
		def makeHandler(): Handler[In, U] = new MappedConsumerHandler(f, self.makeHandler())
	}

	def wrapSafe: Consumer[In, Try[Out]] = Consumer.wrapSafe(this)
	def unwrapSafe[T](implicit ev: Out <:< Try[T]): Consumer[In, T] = Consumer.unwrapSafe(asInstanceOf[Consumer[In, Try[T]]])

}

object Consumer {

	def wrapSafe[In, Out](self: Consumer[In, Out]): Consumer[In, Try[Out]] = new Consumer[In, Try[Out]] {
		def makeHandler(): Handler[In, Try[Out]] = new SafeConsumerHandler(self.makeHandler())
	}

	def unwrapSafe[In, Out](self: Consumer[In, Try[Out]]): Consumer[In, Out] = new Consumer[In, Out] {
		def makeHandler(): Handler[In, Out] = new UnwrapSafeConsumerHandler(self.makeHandler())
	}

	def toList[A]: Consumer[A, List[A]] = new Consumer[A, List[A]] {
		def makeHandler(): Handler[A, List[A]] = {
			new ToListHandler[A]
		}
		override def toString = "ToList"
	}

	def first[A]: Consumer[A, A] = new Consumer[A, A] {
		def makeHandler(): Handler[A, A] = {
			new GetFirstHandler[A]
		}
		override def toString = "First"
	}

	def firstOption[A]: Consumer[A, Option[A]] = new Consumer[A, Option[A]] {
		def makeHandler(): Handler[A, Option[A]] = {
			new GetFirstOptionHandler[A]
		}
		override def toString = "FirstOption"
	}

	def fold[A, R](init: R, f: (R, A) => R): Consumer[A, R] = new Consumer[A, R] {
		def makeHandler(): Handler[A, R] = {
			new FoldHandler(init, f)
		}
		override def toString = s"Fold($init, $f)"
	}

	def foreach[A](f: A => Any): Consumer[A, Unit] = new Consumer[A, Unit] {
		def makeHandler() = new ForEachHandler(f)
		override def toString = s"ForEach($f)"
	}

	def constant[A](result: A): Consumer[Any, A] = new Consumer[Any, A] {
		def makeHandler(): Handler[Any, A] = new ConstantHandler(result)
		override def toString = s"Constant($result)"
	}
}