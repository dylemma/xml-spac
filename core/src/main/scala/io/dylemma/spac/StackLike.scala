package io.dylemma.spac

/** Typeclass that perceives a subset of `In` values as either "stack push" or "stack pop" events.
  * For example, with XML, an `ElemStart` event can be perceived as a "stack push", and a corresponding
  * `ElemEnd` event can be preceived as a "stack pop".
  *
  * @group util
  */
trait StackLike[In, +Elem] {
	def interpretOne(input: In): StackInterpretation[In, Elem]

	def interpret: Transformer[In, Either[ContextChange[In, Elem], In]] = new Transformer.Stateless[In, Either[ContextChange[In, Elem], In]] {
		def push(in: In, out: Transformer.HandlerWrite[Either[ContextChange[In, Elem], In]]) = interpretOne(in) match {
			case StackInterpretation.NoChange => out.push(Right(in))
			case StackInterpretation.ChangedAfterInput(change) => out.push(Right(in)) || out.push(Left(change))
			case StackInterpretation.ChangedBeforeInput(change) => out.push(Left(change)) || out.push(Right(in))
		}
		def finish(out: Transformer.HandlerWrite[Either[ContextChange[In, Elem], In]]): Unit = ()
	}
}
