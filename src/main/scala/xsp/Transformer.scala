package xsp

import javax.xml.stream.events.XMLEvent

import xsp.handlers._

/** An immutable object that can be used to create a handler which wraps
	* an existing handler, possibly transforming inputs before passing them
	* along to the inner handler.
	*/
trait Transformer[In, B] { self =>
	def makeHandler[Out](next: Handler[B, Out]): Handler[In, Out]

	def andThen[C](nextT: Transformer[B, C]): Transformer[In, C] = >>(nextT)
	def >>[C](nextT: Transformer[B, C]): Transformer[In, C] = new Transformer[In, C] {
		def makeHandler[Out](next: Handler[C, Out]): Handler[In, Out] = {
			self.makeHandler(nextT.makeHandler(next))
		}
	}

	def andThen[Out](end: Consumer[B, Out]): Consumer[In, Out] = >>(end)
	def >>[Out](end: Consumer[B, Out]): Consumer[In, Out] = new Consumer[In, Out] {
		def makeHandler(): Handler[In, Out] = {
			self.makeHandler(end.makeHandler())
		}
	}

	def take(n: Int): Transformer[In, B] = andThen(Transformer.Take(n))
	def takeWhile(p: B => Boolean): Transformer[In, B] = andThen(Transformer.TakeWhile(p))
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

	case class FlattenResults[A]() extends Transformer[Result[A], A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[Result[A], Out] = {
			new ResultFlatteningHandler(next)
		}
	}

	case class ExpandResults[A]() extends Transformer[A, Result[A]] {
		def makeHandler[Out](next: Handler[Result[A], Out]): Handler[A, Out] = {
			new ResultExpandingHandler(next)
		}
	}

	case class SideEffect[A](effect: A => Any) extends Transformer[A, A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[A, Out] = {
			new SideEffectHandler(effect, next)
		}
	}
}
