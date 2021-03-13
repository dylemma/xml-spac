package io.dylemma.spac.types

import cats.data.Chain
import cats.~>

/** Typeclass for collections that can be efficiently split into a
  * `head` element and a `tail` collection as long as they are not empty.
  */
trait Unconsable[C[_]] {
	/** Returns either `Some(head -> tail)` or `None` depending on whether the given `coll` is empty.
	  *
	  * @param coll
	  * @tparam A
	  * @return
	  */
	def uncons[A](coll: C[A]): Option[(A, C[A])]
}

object Unconsable {
	def apply[C[_]](implicit C: Unconsable[C]): Unconsable[C] = C

	implicit val catsDataChainUnconsable: Unconsable[Chain] = new Unconsable[Chain] {
		def uncons[A](coll: Chain[A]): Option[(A, Chain[A])] = coll.uncons
	}
	implicit val scalaListUnconsable: Unconsable[List] = new Unconsable[List] {
		def uncons[A](coll: List[A]): Option[(A, List[A])] = coll match {
			case Nil => None
			case head :: tail => Some(head -> tail)
		}
	}
	def fromUncons[C[_]](f: C ~> Lambda[A => Option[(A, C[A])]]): Unconsable[C] = {
		new Unconsable[C] {
			def uncons[A](coll: C[A]) = f(coll)
		}
	}
}
