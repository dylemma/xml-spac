package io.dylemma.xsp

import javax.xml.stream.events.XMLEvent

import io.dylemma.xsp.handlers._

/** An immutable object that can be used to create a handler which wraps
	* an existing handler, possibly transforming inputs before passing them
	* along to the downstream handler.
	*/
trait Transformer[In, B] { self =>
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
	def scan[S](init: S)(f: (S, B) => S): Transformer[In, S] = andThen(Transformer.Scan(init)(f))
	def filter(p: B => Boolean): Transformer[In, B] = andThen(Transformer.Filter(p))
	def flattenResults[T](implicit ev: B =:= Result[T]): Transformer[In, T] = {
		asInstanceOf[Transformer[In, Result[T]]].andThen(Transformer.FlattenResults[T]())
	}
	def expandResults: Transformer[In, Result[B]] = andThen(Transformer.ExpandResults())
	def withSideEffect(effect: B => Any): Transformer[In, B] = andThen(Transformer.SideEffect(effect))

	def consumeToList: Consumer[In, Result[List[B]]] = andThen(Consumer.ToList())
	def consumeFirst: Consumer[In, Result[B]] = andThen(Consumer.First())
	def consumeFirstOption: Consumer[In, Result[Option[B]]] = andThen(Consumer.FirstOption())
	def consumeAsFold[R](init: R)(f: (R, B) => R): Consumer[In, Result[R]] = andThen(Consumer.Fold(init, f))
	def consumeAsResultFold[R](init: Result[R])(
		f: (Result[R], Result[B]) => Result[R]
	): Consumer[In, Result[R]] = andThen(Consumer.FoldResults(init, f))
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

	case class FlattenResults[A]() extends Transformer[Result[A], A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[Result[A], Out] = {
			new ResultFlatteningHandler(next)
		}
		override def toString = "FlattenResults"
	}

	case class ExpandResults[A]() extends Transformer[A, Result[A]] {
		def makeHandler[Out](next: Handler[Result[A], Out]): Handler[A, Out] = {
			new ResultExpandingHandler(next)
		}
		override def toString = "ExpandResults"
	}

	case class SideEffect[A](effect: A => Any) extends Transformer[A, A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[A, Out] = {
			new SideEffectHandler(effect, next)
		}
		override def toString = s"SideEffect($effect)"
	}
}
