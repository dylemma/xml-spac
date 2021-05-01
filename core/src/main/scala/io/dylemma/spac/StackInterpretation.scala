package io.dylemma.spac

/** Outcome of a `StackLike[In, Elem]`, indicating whether a given input was a stack push/pop,
  * and whether that push/pop should be treated as happening before or after the input that caused it.
  *
  * @tparam In
  * @tparam Elem
  */
sealed trait StackInterpretation[+In, +Elem]
object StackInterpretation {
	case object NoChange extends StackInterpretation[Nothing, Nothing]
	case class ChangedAfterInput[In, Elem](change: ContextChange[In, Elem]) extends StackInterpretation[In, Elem]
	case class ChangedBeforeInput[In, Elem](change: ContextChange[In, Elem]) extends StackInterpretation[In, Elem]

	object AnyChange {
		def unapply[In, Elem](a: StackInterpretation[In, Elem]) = a match {
			case NoChange => None
			case ChangedBeforeInput(change) => Some(change)
			case ChangedAfterInput(change) => Some(change)
		}
	}
}
