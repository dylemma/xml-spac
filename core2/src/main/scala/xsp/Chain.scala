package xsp

/** A left-associative heterogenous list (like shapeless.HList, but in reverse).
	* Chains start with a `Start`, then use `~` to append arbitrary other elements.
	* Appending a single element is O(1).
	*/
sealed trait Chain
final case class ~[+H <: Chain, +T](head: H, tail: T) extends Chain {
	def ~[T](tail: T) = new ~(this, tail)
	override def toString = s"$head ~ $tail"
}
sealed trait Start extends Chain {
	override def toString = "Start"
	def ~[T](tail: T) = new ~(this, tail)
}
case object Start extends Start

object ChainOps {

	// Concat
	trait Concat[-A <: Chain, -B <: Chain, +C <: Chain] {
		def apply(a: A, b: B): C
	}
	object Concat {
		// (Chain ~ A ~ B) ++ (Chain ~ C ~ D) = (Chain ~ A ~ B ~ C ~ D)

		// (any chain) ++ (Start) = (any chain)
		implicit def identity[A <: Chain] = new Concat[A, Start, A] {
			def apply(a: A, b: Start): A = a
		}

		// (A) ++ (BH ~ BT) = (A ++ BH) ~ BT
		implicit def associative[A <: Chain, BH <: Chain, BT, Inner <: Chain](implicit concat: Concat[A, BH, Inner]) = {
			new Concat[A, BH ~ BT, Inner ~ BT] {
				def apply(a: A, b: ~[BH, BT]): ~[Inner, BT] = {
					new ~(concat(a, b.head), b.tail)
				}
			}
		}
	}

	implicit class ConcatOps[A <: Chain](a: A) {
		def ++[B <: Chain, C <: Chain](b: B)(implicit concat: Concat[A, B, C]): C = concat(a, b)
	}

	sealed trait NonChain[-A]
	object NonChain {
		implicit def anyNonChain[A]: NonChain[A] = null
		implicit def chainAmbiguity[A <: Chain]: NonChain[A] = null
	}

	trait Rep[T, C <: Chain] {
		def toChain(t: T): C
		def fromChain(c: C): T
	}
	object Rep {
		implicit object UnitRep extends Rep[Unit, Start] {
			def toChain(t: Unit) = Start
			def fromChain(c: Start) = ()
		}
		// unfortunately this works on tuples and unit...
		implicit def nonChainRep[A: NonChain] = new Rep[A, Start ~ A] {
			def toChain(a: A) = Start ~ a
			def fromChain(c: Start ~ A) = c.tail
		}
		implicit def chainRep2[T1, T2] = new Rep[(T1, T2), Start ~ T1 ~ T2] {
			def toChain(t: (T1, T2)) = Start ~ t._1 ~ t._2
			def fromChain(c: Start ~ T1 ~ T2) = c match {
				case Start ~ t1 ~ t2 => (t1, t2)
			}
		}
		implicit def chainRep3[T1, T2, T3] = new Rep[(T1, T2, T3), Start ~ T1 ~ T2 ~ T3] {
			def toChain(t: (T1, T2, T3)) = Start ~ t._1 ~ t._2 ~ t._3
			def fromChain(c: Start ~ T1 ~ T2 ~ T3) = c match {
				case Start ~ t1 ~ t2 ~ t3 => (t1, t2, t3)
			}
		}

		def apply[T] = new RepApply[T]
		class RepApply[T] {
			def apply[C <: Chain]()(implicit rep: Rep[T, C]) = rep
		}

		def from[C <: Chain] = new FromApply[C]
		class FromApply[C <: Chain] {
			def apply[T]()(implicit rep: Rep[T, C]) = rep
		}
	}

}
