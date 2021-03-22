package io.dylemma

import cats.data.Chain
import cats.effect.{Resource, SyncIO}

import scala.collection.AbstractIterator

package object spac {

	type Emit[+A] = Chain[A]
	val Emit: Chain.type = Chain

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
