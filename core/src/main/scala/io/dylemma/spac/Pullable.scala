package io.dylemma.spac

import cats.data.{Chain, NonEmptyChain}
import cats.effect.{Resource, SyncIO}
import cats.implicits._
import cats.{Applicative, Monad}
import io.dylemma.spac.types.Unconsable

import scala.collection.AbstractIterator

trait ToPullable[F[+_], S, +A] {
	def apply(source: S): Resource[F, Pullable[F, A]]
}
object ToPullable {
	implicit def forUnconsable[F[+_] : Applicative, C[_] : Unconsable, A]: ToPullable[F, C[A], A] = new ToPullable[F, C[A], A] {
		def apply(source: C[A]) = Resource.liftF {
			source.pure[F].map { Pullable.fromUnconsable[F, C, A] }
		}
	}

	object Ops extends Ops
	trait Ops {
		implicit class SourceToPullable[S](source: S) {
			/** Returns a resource which allocates a "Pullable" view of this `source`.
			  *
			  * @param S
			  * @tparam F
			  * @tparam A
			  * @return
			  */
			def toPullable[F[+_], A](implicit S: ToPullable[F, S, A]): Resource[F, Pullable[F, A]] = S(source)

			/** Returns a resource which allocates an Iterator[A] view of this `source`.
			  */
			def toIterator[A](implicit S: ToPullable[SyncIO, S, A]): Resource[SyncIO, Iterator[A]] = S(source).map { pullable =>
				// the "close" is taken care of by the `Resource` that wraps this, so we're not exposing the `AutoCloseable` aspect of the iterator in this case
				new SyncPullableIterator[A](Some(pullable), SyncIO.unit)
			}

			/** Returns a closeable iterator over the events in this `source`.
			  * If you use this method on a `source` that has some underlying resource allocation step,
			  * you *MUST* call this Iterator's `close()` method so that the underlying resource can be released.
			  *
			  * @param S
			  * @tparam A
			  * @return
			  */
			def toCloseableIterator[A](implicit S: ToPullable[SyncIO, S, A]): Iterator[A] with AutoCloseable = {
				val (pullable, closeIO) = S(source).allocated.unsafeRunSync()
				new SyncPullableIterator[A](Some(pullable), closeIO)
			}
		}
	}

	private class SyncPullableIterator[A](private var state: Option[Pullable[SyncIO, A]], closeIO: SyncIO[Unit]) extends AbstractIterator[A] with AutoCloseable {
		private var nextResult: Option[A] = None
		def hasNext = nextResult match {
			case Some(_) => true
			case None => doPull()
		}

		def next() = nextResult match {
			case Some(a) =>
				nextResult = None
				a
			case None =>
				if (doPull()) nextResult.get
				else throw new NoSuchElementException("next() on empty iterator")
		}

		def close() = closeIO.unsafeRunSync()

		// precondition: `nextResult` is None
		// postcondition: either `nextResult` is Some or `state` is None
		private def doPull(): Boolean = state match {
			case Some(p) =>
				val unconsed = p.uncons.unsafeRunSync()
				nextResult = unconsed.map(_._1)
				state = unconsed.map(_._2)
				nextResult.isDefined
			case None =>
				false
		}
	}
}

trait Pullable[F[+_], +A] {
	def uncons: F[Option[(A, Pullable[F, A])]]

	def into[B](parser: Parser[A, B])(implicit F: Monad[F]): F[B] = F.tailRecM(this -> parser.newHandler) { case (currentPull, handler) =>
		currentPull.uncons.map {
			case None => Right(handler.finish())
			case Some((a, nextPull)) => handler.step(a) match {
				case Left(result) => Right(result)
				case Right(nextParser) => Left(nextPull -> nextParser)
			}
		}
	}

	def through[B](transformer: Transformer[A, B])(implicit F: Monad[F]): Pullable[F, B] = {
		new TransformedPullable(this, transformer.newHandler)
	}
}
object Pullable {
	def apply[F[+_], A] = new PullablePartialApply[F, A]
	class PullablePartialApply[F[+_], A] {
		def from[S](source: S)(implicit toPullable: ToPullable[F, S, A]): Resource[F, Pullable[F, A]] = toPullable(source)
	}

	def nil[F[+_] : Applicative]: Pullable[F, Nothing] = new Pullable[F, Nothing] {
		def uncons = None.pure[F]
	}

	def cons[F[+_] : Applicative, A](toEmit: Chain[A], next: Pullable[F, A]): Pullable[F, A] = new Pullable[F, A] {
		def uncons = unconsFrom(toEmit, next)
	}
	def unconsFrom[F[+_] : Applicative, A](pendingEmit: Chain[A], next: Pullable[F, A]): F[Option[(A, Pullable[F, A])]] = pendingEmit.uncons match {
		case None => next.uncons
		case Some((head, tail)) =>
			val nextPull = NonEmptyChain.fromChain(tail) match {
				case None => next
				case Some(tailBuffer) => new BufferedPullable[F, A](tailBuffer, next)
			}
			Some(head -> nextPull).pure[F]
	}

	def fromUnconsable[F[+_] : Applicative, C[_] : Unconsable, A](c: C[A]): Pullable[F, A] = {
		new UnconsableWrapper(c.pure[F])
	}
	private class UnconsableWrapper[F[+_], C[_], A](seqF: F[C[A]])(implicit F: Applicative[F], C: Unconsable[C]) extends Pullable[F, A] {
		def uncons: F[Option[(A, UnconsableWrapper[F, C, A])]] = F.map(seqF) { seq =>
			C.uncons(seq) map { case (head, tail) =>
				head -> new UnconsableWrapper(tail.pure[F])
			}
		}
	}
}

private class TransformedPullable[F[+_] : Monad, A, B](base: Pullable[F, A], transformer: Transformer.Handler[A, B]) extends Pullable[F, B] {
	def uncons = base.uncons.flatMap {
		case Some((head, nextPull)) =>
			transformer.step(head) match {
				// if the transformer ends, we can discard the `nextPull` and end after emitting the `toEmit` buffer
				case (toEmit, None) => Pullable.unconsFrom(toEmit, Pullable.nil[F])
				// otherwise, emit the `toEmit` buffer before continuing on with the 'next' states
				case (toEmit, Some(nextTransformer)) => Pullable.unconsFrom(toEmit, new TransformedPullable(nextPull, nextTransformer))
			}
		case None =>
			// EOF - finish the transformer and emit whatever leftovers it produced
			val emit = transformer.finish()
			Pullable.unconsFrom(emit, Pullable.nil[F])
	}
}
private class BufferedPullable[F[+_] : Applicative, A](buffer: NonEmptyChain[A], next: Pullable[F, A]) extends Pullable[F, A] {
	def uncons = Some((
		buffer.head,
		NonEmptyChain.fromChain(buffer.tail) match {
			case None => next
			case Some(tail) => new BufferedPullable(tail, next)
		}
	)).pure[F]
}