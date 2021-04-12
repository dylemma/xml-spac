package io.dylemma.spac
package json
package impl

/** Special transformer that adds `IndexStart`, `IndexEnd`, and `FieldEnd` events in between the usual
  * JSON token-related events that you'd normally get from a JSON parser.
  * The point of this is to make the JSON events reliably "stack-like", where anything that pushes
  * to a context (like starting an object field) gets a corresponding "pop" event even though such
  * and event isn't represented by any particular token in the JSON syntax.
  * Having these "virtual" events occur is necessary to make JsonParsers in spac function properly.
  * Users of the spac library shouldn't have to use this class; support libraries (like json-spac-jackson)
  * should include this functionality as part of the `AsPullable` typeclass that they provide.
  */
object JsonPopEventInjector extends Transformer[JsonEvent, JsonEvent] {
	def newHandler = new Handler(Nil)

	class Handler(val contextStack: List[JsonStackElem]) extends Transformer.Handler[JsonEvent, JsonEvent] {
		import JsonEvent._
		def step(in: JsonEvent) = {
			val toEmit = contextStack.headOption match {
				case Some(ArrayStart()) if !in.isArrayEnd =>
					// If an array just started, any event but ArrayEnd should push an Index(0) context.
					val s = IndexStart(0, in.location)
					Emit(s, in) //-> Some(new Handler(s :: contextStack))

				case Some(IndexStart(s)) if !in.isArrayEnd =>
					// If we're in the middle of an array, any event but ArrayEnd should advance the index.
					val e = IndexEnd(in.location)
					val c = IndexStart(s.index + 1, in.location)
					Emit(e, c, in) //-> Some(new Handler(c :: contextStack.tail))

				case Some(IndexStart(_)) if in.isArrayEnd =>
					// If we're inside an array index when the array ends, inject an IndexEnd first.
					Emit(IndexEnd(in.location), in) //-> Some(new Handler(contextStack.tail))

				case Some(FieldStart(_)) if in.asFieldStart.isDefined =>
					// If we encounter a new field while already in a field, inject a FieldEnd first
					Emit(FieldEnd(in.location), in)

				case Some(FieldStart(_)) if in.isObjectEnd =>
					// If the object ends while we're in a field, inject a FieldEnd first
					Emit(FieldEnd(in.location), in)

				case _ =>
					// All other events can be passed through normally
					Emit.one(in)
			}

			val stackAfter = toEmit.foldLeft(contextStack){ (stack, e) =>
				e match {
					case se: JsonStackElem => se :: stack
					case p: JsonStackPop => stack.tail
					case _ => stack
				}
			}

			toEmit -> Some(new Handler(stackAfter))
		}

		def finish() = {
			// in theory the stack should be empty if the JSON terminates naturally,
			// but if for some reason it doesn't, we'll inject "opposite" events to close out whatever stack state remains
			def unwind(remainingStack: List[JsonStackElem]): Emit[JsonEvent] = remainingStack match {
				case (_: ArrayStart) :: tail => ArrayEnd(ContextLocation.empty) +: unwind(tail)
				case (_: ObjectStart) :: tail => ObjectEnd(ContextLocation.empty) +: unwind(tail)
				case (_: FieldStart) :: tail => FieldEnd(ContextLocation.empty) +: unwind(tail)
				case (_: IndexStart) :: tail => IndexEnd(ContextLocation.empty) +: unwind(tail)
				case Nil => Emit.nil
			}
			unwind(contextStack)
		}
	}
}
