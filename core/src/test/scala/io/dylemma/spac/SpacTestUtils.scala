package io.dylemma.spac

import cats.~>
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
//	class UnconsCountingArbitrarySequence[A](implicit A: Arbitrary[A]) {
//		private var _unconsCounter = 0
//		private var _current: Option[A] = None
//		def doUncons() = {
//			_unconsCounter += 1
//			_current = A.arbitrary.sample
//		}
//		def head = _current.get
//		def unconsCount = _unconsCounter
//	}
//	object UnconsCountingArbitrarySequence {
//		implicit val asUnconsable: Unconsable[UnconsCountingArbitrarySequence] = new Unconsable[UnconsCountingArbitrarySequence] {
//			def uncons[A](coll: UnconsCountingArbitrarySequence[A]) = {
//				coll.doUncons()
//				Some(coll.head -> coll)
//			}
//		}
//	}

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

//	trait UnconsCountingWrappedSequence[A] {
//		def unconsCount: Int
//		def doUncons(): Option[(A, UnconsCountingWrappedSequence[A])]
//	}
//	object UnconsCountingWrappedSequence {
//		implicit def asUnconsable: Unconsable[UnconsCountingWrappedSequence] = {
//			new Unconsable[UnconsCountingWrappedSequence] {
//				def uncons[A](coll: UnconsCountingWrappedSequence[A]) = coll.doUncons()
//			}
//		}
//		class Instance[C[_], A](original: C[A])(implicit C: Unconsable[C]) extends UnconsCountingWrappedSequence[A] {
//			private var _unconsCounter = 0
//			private var _current: Option[C[A]] = Some(original)
//			def unconsCount = _unconsCounter
//			def doUncons() = {
//				_unconsCounter += 1
//				_current.flatMap { seq =>
//					val innerUncons = Unconsable[C].uncons(seq)
//					_current = innerUncons.map(_._2)
//					innerUncons map { _._1 -> this }
//				}
//			}
//		}
//	}

	def tryFlattenTuple(maybeTuple: Any): List[Any] = maybeTuple match {
		case (a, b, c, d, e, f) => List(a, b, c, d, e, f).flatMap(tryFlattenTuple)
		case (a, b, c, d, e) => List(a, b, c, d, e).flatMap(tryFlattenTuple)
		case (a, b, c, d) => List(a, b, c, d).flatMap(tryFlattenTuple)
		case (a, b, c) => List(a, b, c).flatMap(tryFlattenTuple)
		case (a, b) => List(a, b).flatMap(tryFlattenTuple)
		case a => a :: Nil
	}
}
