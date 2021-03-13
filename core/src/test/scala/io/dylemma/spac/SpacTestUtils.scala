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

	def countPullsIn[C[_]: Unconsable, A](seq: C[A])(sideEffect: UnconsCountingWrappedSequence[C, A] => Unit): Int = {
		val wrappedSeq = new UnconsCountingWrappedSequence(seq)
		sideEffect(wrappedSeq)
		wrappedSeq.unconsCount
	}
	case class UnconsCountingWrappedSequence[C[_]: Unconsable, A](original: C[A]) {
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
	object UnconsCountingWrappedSequence {
		implicit def asUnconsable[C[_]: Unconsable]: Unconsable[UnconsCountingWrappedSequence[C, *]] = {
			new Unconsable[UnconsCountingWrappedSequence[C, *]] {
				def uncons[A](coll: UnconsCountingWrappedSequence[C, A]) = coll.doUncons()
			}
		}
	}
}
