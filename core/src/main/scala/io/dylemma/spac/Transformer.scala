package io.dylemma.spac

import io.dylemma.spac.handlers._
import io.dylemma.spac.types.Stackable

import scala.util.Try

/** An immutable object that can be used to create a handler which wraps
	* an existing handler, possibly transforming inputs before passing them
	* along to the downstream handler.
	*/
trait Transformer[-In, +B] { self =>
	def makeHandler[Out](next: Handler[B, Out]): Handler[In, Out]

	def andThen[C](nextT: Transformer[B, C]): Transformer[In, C] = >>(nextT)
	def >>[C](nextT: Transformer[B, C]): Transformer[In, C] = new Transformer[In, C] {
		def makeHandler[Out](next: Handler[C, Out]): Handler[In, Out] = {
			self.makeHandler(nextT.makeHandler(next))
		}
		override def toString = s"$self >> $nextT"
	}

	def andThen[Out](end: Consumer[B, Out]): Consumer[In, Out] = >>(end)
	def >>[Out](end: Consumer[B, Out]): Consumer[In, Out] = new Consumer[In, Out] {
		def makeHandler(): Handler[In, Out] = {
			self.makeHandler(end.makeHandler())
		}
		override def toString = s"$self >> $end"
	}

	def take(n: Int): Transformer[In, B] = andThen(Transformer.Take(n))
	def takeWhile(p: B => Boolean): Transformer[In, B] = andThen(Transformer.TakeWhile(p))
	def drop(n: Int): Transformer[In, B] = andThen(Transformer.Drop(n))
	def dropWhile(p: B => Boolean): Transformer[In, B] = andThen(Transformer.DropWhile(p))
	def map[C](f: B => C): Transformer[In, C] = andThen(Transformer.Map(f))
	def collect[C](pf: PartialFunction[B, C]): Transformer[In, C] = andThen(Transformer.Collect(pf))
	def scan[S](init: S)(f: (S, B) => S): Transformer[In, S] = andThen(Transformer.Scan(init)(f))
	def filter(p: B => Boolean): Transformer[In, B] = andThen(Transformer.Filter(p))
	def withFilter(p: B => Boolean): Transformer[In, B] = andThen(Transformer.Filter(p))
	def unwrapSafe[T](implicit ev: B <:< Try[T]): Transformer[In, T] = {
		asInstanceOf[Transformer[In, Try[T]]].andThen(Transformer.UnwrapSafe[T]())
	}
	def wrapSafe: Transformer[In, Try[B]] = andThen(Transformer.WrapSafe())
	def withSideEffect(effect: B => Any): Transformer[In, B] = andThen(Transformer.SideEffect(effect))

	def consumeToList: Consumer[In, List[B]] = andThen(Consumer.ToList())
	def consumeFirst: Consumer[In, B] = andThen(Consumer.First())
	def consumeFirstOption: Consumer[In, Option[B]] = andThen(Consumer.FirstOption())
	def consumeAsFold[R](init: R)(f: (R, B) => R): Consumer[In, R] = andThen(Consumer.Fold(init, f))
	def consumeForEach(f: B => Any): Consumer[In, Unit] = andThen(Consumer.ForEach(f))
}

object Transformer {
	case class Take[A](max: Int) extends Transformer[A, A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[A, Out] = {
			new TakeNHandler[A, Out](max, next)
		}
		override def toString = s"Take($max)"
	}

	case class TakeWhile[A](p: A => Boolean) extends Transformer[A, A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[A, Out] = {
			new TakeWhileHandler[A, Out](p, next)
		}
		override def toString = s"TakeWhile($p)"
	}

	case class Drop[A](numToDrop: Int) extends Transformer[A, A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[A, Out] = {
			new DropNHandler(numToDrop, next)
		}
		override def toString = s"Drop($numToDrop)"
	}

	case class DropWhile[A](p: A => Boolean) extends Transformer[A, A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[A, Out] = {
			new DropWhileHandler(p, next)
		}
		override def toString = s"DropWhile($p)"
	}

	case class Filter[A](p: A => Boolean) extends Transformer[A, A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[A, Out] = {
			new FilteringHandler[A, Out](p, next)
		}
		override def toString = s"Filter($p)"
	}

	case class Map[A, B](f: A => B) extends Transformer[A, B] {
		def makeHandler[Out](next: Handler[B, Out]): Handler[A, Out] = {
			new MappedTransformerHandler(f, next)
		}
		override def toString = s"Map($f)"
	}

	case class Collect[A, B](pf: PartialFunction[A, B]) extends Transformer[A, B] {
		def makeHandler[Out](next: Handler[B, Out]): Handler[A, Out] = {
			new CollectHandler(pf, next)
		}
	}

	case class Scan[S, A](init: S)(f: (S, A) => S) extends Transformer[A, S] {
		def makeHandler[Out](next: Handler[S, Out]): Handler[A, Out] = {
			new ScanningHandler(init, f, next)
		}
	}

	case class UnwrapSafe[A]() extends Transformer[Try[A], A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[Try[A], Out] = {
			new UnwrapSafeTransformerHandler(next)
		}
		override def toString = "UnwrapSafe"
	}

	case class WrapSafe[A]() extends Transformer[A, Try[A]] {
		def makeHandler[Out](next: Handler[Try[A], Out]): Handler[A, Out] = {
			new WrapSafeTransformerHandler(next)
		}
		override def toString = "WrapSafe"
	}

	case class SideEffect[A](effect: A => Any) extends Transformer[A, A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[A, Out] = {
			new SideEffectHandler(effect, next)
		}
		override def toString = s"SideEffect($effect)"
	}

	case class Sequenced[In: Stackable, T1, T2](consumer: Consumer[In, T1], getTransformer: T1 => Transformer[In, T2]) extends Transformer[In, T2] {
		def makeHandler[Out](next: Handler[T2, Out]): Handler[In, Out] = {
			val handler1 = consumer.makeHandler()
			def getHandler2(h1Result: T1) = getTransformer(h1Result).makeHandler(next)
			new SequencedInStackHandler(handler1, getHandler2)
		}
	}
}
