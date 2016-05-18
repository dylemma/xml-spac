package xsp

import scala.annotation.implicitNotFound

/** Evidence that type `A` is the most specific type between `B1` and `B2`,
	* and that `B1` and `B2` both belong to the same type hierarchy. In other
	* words, `B1 <: B2` or `B2 <: B1`.
	*
	* @tparam A The common type
	* @tparam B1
	* @tparam B2
	*/
@implicitNotFound("There is no common type between ${B1} and ${B2}")
sealed abstract class MostSpecificType[A, B1, B2] extends (A => (B1, B2))

object MostSpecificType {
	private def instance[A, B1, B2](conv1: A => B1, conv2: A => B2) = new MostSpecificType[A, B1, B2]{
		def apply(a: A): (B1, B2) = (conv1(a), conv2(a))
	}

	implicit def sameType[A]: MostSpecificType[A, A, A] = instance(identity, identity)
	implicit def moreSpecificLeft[B, A <: B]: MostSpecificType[A, A, B] = instance(identity, identity)
	implicit def moreSpecificRight[A, B <: A]: MostSpecificType[B, A, B] = instance(identity, identity)

	def between[B1, B2] = new Between[B1, B2]
	class Between[B1, B2] {
		def apply[A]()(implicit mst: MostSpecificType[A, B1, B2]) = mst
	}
}
