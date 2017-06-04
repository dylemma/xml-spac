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
trait TypeReduce[-In] {
	type Out
	def apply(in: In): Out
}

object TypeReduce extends LowPriorityTypeReduceImplicits {
	type Aux[In, O2] = TypeReduce[In] { type Out = O2 }

	implicit val flattenTwoUnits: Aux[(Unit, Unit), Unit] = new TypeReduce[(Unit, Unit)] {
		type Out = Unit
		def apply(v: (Unit, Unit)) = ()
	}
	implicit def flattenLeftUnit[T]: Aux[(Unit, T), T] = new TypeReduce[(Unit, T)] {
		type Out = T
		def apply(v: (Unit, T)) = v._2
	}
	implicit def flattenRightUnit[T]: Aux[(T, Unit), T] = new TypeReduce[(T, Unit)] {
		type Out = T
		def apply(v: (T, Unit)) = v._1
	}
}
trait LowPriorityTypeReduceImplicits {
	implicit def noopFlatten[L, R]: TypeReduce.Aux[(L, R), (L, R)] = new TypeReduce[(L, R)] {
		type Out = (L, R)
		def apply(v: (L, R)) = v
	}
}
