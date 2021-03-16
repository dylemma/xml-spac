package io.dylemma.spac

import cats.~>
import io.dylemma.spac.types.Unconsable
import org.scalacheck.Arbitrary

trait SpacTestUtils {
	def countPullsIn[A: Arbitrary](sideEffect: UnconsCountingArbitrarySequence[A] => Unit): Int = {
		val seq = new UnconsCountingArbitrarySequence[A]
		sideEffect(seq)
		seq.unconsCount
	}
	class UnconsCountingArbitrarySequence[A](implicit A: Arbitrary[A]) {
		private var _unconsCounter = 0
		private var _current: Option[A] = None
		def doUncons() = {
			_unconsCounter += 1
			_current = A.arbitrary.sample
		}
		def head = _current.get
		def unconsCount = _unconsCounter
	}
	object UnconsCountingArbitrarySequence {
		implicit val asUnconsable: Unconsable[UnconsCountingArbitrarySequence] = new Unconsable[UnconsCountingArbitrarySequence] {
			def uncons[A](coll: UnconsCountingArbitrarySequence[A]) = {
				coll.doUncons()
				Some(coll.head -> coll)
			}
		}
	}

	def countPullsIn[C[_]: Unconsable, A](seq: C[A])(sideEffect: UnconsCountingWrappedSequence[A] => Unit): Int = {
		val wrappedSeq = new UnconsCountingWrappedSequence.Instance(seq)
		sideEffect(wrappedSeq)
		wrappedSeq.unconsCount
	}
	trait UnconsCountingWrappedSequence[A] {
		def unconsCount: Int
		def doUncons(): Option[(A, UnconsCountingWrappedSequence[A])]
	}
	object UnconsCountingWrappedSequence {
		implicit def asUnconsable: Unconsable[UnconsCountingWrappedSequence] = {
			new Unconsable[UnconsCountingWrappedSequence] {
				def uncons[A](coll: UnconsCountingWrappedSequence[A]) = coll.doUncons()
			}
		}
		class Instance[C[_], A](original: C[A])(implicit C: Unconsable[C]) extends UnconsCountingWrappedSequence[A] {
			private var _unconsCounter = 0
			private var _current: Option[C[A]] = Some(original)
			def unconsCount = _unconsCounter
			def doUncons() = {
				_unconsCounter += 1
				_current.flatMap { seq =>
					val innerUncons = Unconsable[C].uncons(seq)
					_current = innerUncons.map(_._2)
					innerUncons map { _._1 -> this }
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
