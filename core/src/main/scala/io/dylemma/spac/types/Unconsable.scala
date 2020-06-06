package io.dylemma.spac.types

import cats.data.Chain

/** Typeclass for collections that can be efficiently split into a
  * `head` element and a `tail` collection as long as they are not empty.
  */
trait Unconsable[C[_]] {
	def uncons[A](coll: C[A]): Option[(A, C[A])]
}

object Unconsable {
	implicit val catsDataChainUnconsable: Unconsable[Chain] = new Unconsable[Chain] {
		def uncons[A](coll: Chain[A]): Option[(A, Chain[A])] = coll.uncons
	}
	implicit val scalaListUnconsable: Unconsable[List] = new Unconsable[List] {
		def uncons[A](coll: List[A]): Option[(A, List[A])] = coll match {
			case Nil => None
			case head :: tail => Some(head -> tail)
		}
	}
}
