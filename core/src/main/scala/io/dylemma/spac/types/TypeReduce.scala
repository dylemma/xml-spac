package io.dylemma.spac.types

/** Type-level tuple reduction function that treats `Unit` as an Identity.
	* For example:
	* {{{
	*   TypeReduce.Aux[(Unit, Unit)]{ type Out = Unit }
	*   TypeReduce.Aux[(T, Unit)]{ type Out = T }
	*   TypeReduce.Aux[(Unit, T)]{ type Out = T }
	*   TypeReduce.Aux[(L, R)]{ type Out = (L, R) }
	* }}}
	*/
trait TypeReduce[-In1, -In2] {
	type Out
	def apply(in1: In1, in2: In2): Out
}

object TypeReduce extends LowPriorityTypeReduceImplicits {
	type Aux[In1, In2, O2] = TypeReduce[In1, In2] { type Out = O2 }

	implicit val flattenTwoUnits: Aux[Unit, Unit, Unit] = new TypeReduce[Unit, Unit] {
		type Out = Unit
		def apply(in1: Unit, in2: Unit): Unit = ()
	}
	implicit def flattenLeftUnit[T]: Aux[Unit, T, T] = new TypeReduce[Unit, T] {
		type Out = T
		def apply(in1: Unit, in2: T): T = in2
	}
	implicit def flattenRightUnit[T]: Aux[T, Unit, T] = new TypeReduce[T, Unit] {
		type Out = T
		def apply(in1: T, in2: Unit): T = in1
	}
}
trait LowPriorityTypeReduceImplicits {
	implicit def noopFlatten[L, R]: TypeReduce.Aux[L, R, (L, R)] = new TypeReduce[L, R] {
		type Out = (L, R)
		def apply(in1: L, in2: R): (L, R) = (in1, in2)
	}
}
