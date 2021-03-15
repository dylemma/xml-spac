package io.dylemma.spac

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
