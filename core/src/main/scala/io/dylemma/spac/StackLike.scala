package io.dylemma.spac

/** Typeclass that perceives a subset of `In` values as either "stack push" or "stack pop" events.
  * For example, with XML, an `ElemStart` event can be perceived as a "stack push", and a corresponding
  * `ElemEnd` event can be preceived as a "stack pop".
  *
  * @group util
  */
trait StackLike[In, +Elem] {
	def interpretOne(input: In): StackInterpretation[In, Elem]

	def interpret: Transformer[In, Either[ContextChange[In, Elem], In]] = Transformer[In].op { in =>
		interpretOne(in) match {
			case StackInterpretation.NoChange => Emit.one(Right(in))
			case StackInterpretation.ChangedAfterInput(change) => Emit(Right(in), Left(change))
			case StackInterpretation.ChangedBeforeInput(change) => Emit(Left(change), Right(in))
		}
	}
}
