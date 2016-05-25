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

/** Typeclass for concatenating chains.
	*
	* @tparam A The left operand (prefix) type
	* @tparam B The right operand (suffix) type
	* @tparam C The result type
	*/
trait ChainConcat[-A <: Chain, -B <: Chain, +C <: Chain] {
	def apply(a: A, b: B): C
}
object ChainConcat {
	// (Chain ~ A ~ B) ++ (Chain ~ C ~ D) = (Chain ~ A ~ B ~ C ~ D)

	// (any chain) ++ (Start) = (any chain)
	implicit def identity[A <: Chain] = new ChainConcat[A, Start, A] {
		def apply(a: A, b: Start): A = a
	}

	// (A) ++ (BH ~ BT) = (A ++ BH) ~ BT
	implicit def associative[A <: Chain, BH <: Chain, BT, Inner <: Chain](implicit concat: ChainConcat[A, BH, Inner]) = {
		new ChainConcat[A, BH ~ BT, Inner ~ BT] {
			def apply(a: A, b: ~[BH, BT]): ~[Inner, BT] = {
				new ~(concat(a, b.head), b.tail)
			}
		}
	}
}
