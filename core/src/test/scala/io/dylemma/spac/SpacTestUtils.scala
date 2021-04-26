package io.dylemma.spac

import org.scalacheck.Arbitrary

trait SpacTestUtils {
	object countPullsIn {
		def arbitrary[A: Arbitrary](sideEffect: PullCountingIterable[A] => Unit): Int = {
			val seq = PullCountingIterable.arbitrary[A]
			sideEffect(seq)
			seq.pullCount
		}
		def apply[A](seq: Iterable[A])(sideEffect: PullCountingIterable[A] => Unit): Int = {
			val wrappedSeq = PullCountingIterable.wrap(seq)
			sideEffect(wrappedSeq)
			wrappedSeq.pullCount
		}
	}

	trait PullCountingIterable[A] extends Iterable[A] {
		def pullCount: Int
	}
	object PullCountingIterable {
		def wrap[A](inner: Iterable[A]): PullCountingIterable[A] = new WrappedInstance[A](inner)
		def arbitrary[A: Arbitrary]: PullCountingIterable[A] = new ArbitraryInstance[A]

		class WrappedInstance[A](inner: Iterable[A]) extends PullCountingIterable[A] {
			private var _pullCount = 0
			def pullCount = _pullCount
			def iterator: Iterator[A] = new Iterator[A] {
				val i = inner.iterator
				def hasNext = i.hasNext
				def next() = {
					_pullCount += 1
					i.next()
				}
			}
		}
		class ArbitraryInstance[A](implicit A: Arbitrary[A]) extends PullCountingIterable[A] {
			private var _pullCount = 0
			def pullCount = _pullCount
			def iterator: Iterator[A] = new Iterator[A] {
				def hasNext = true
				def next() = {
					_pullCount += 1
					A.arbitrary.sample.get
				}
			}
		}
	}

	def tryFlattenTuple(maybeTuple: Any): List[Any] = maybeTuple match {
		case (a, b, c, d, e, f) => List(a, b, c, d, e, f).flatMap(tryFlattenTuple)
		case (a, b, c, d, e) => List(a, b, c, d, e).flatMap(tryFlattenTuple)
		case (a, b, c, d) => List(a, b, c, d).flatMap(tryFlattenTuple)
		case (a, b, c) => List(a, b, c).flatMap(tryFlattenTuple)
		case (a, b) => List(a, b).flatMap(tryFlattenTuple)
		case a => a :: Nil
	}
}
