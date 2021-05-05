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
object JsonStackFixer extends Transformer[JsonEvent, JsonEvent] {
	def newHandler = new Handler

	class Handler extends Transformer.Handler[JsonEvent, JsonEvent] {

		import JsonEvent._
		private var contextStack: List[JsonStackElem] = Nil
		private var lastEventLocation = ContextLocation.empty

		def push(in: JsonEvent, out: Transformer.HandlerWrite[JsonEvent]) = {
			@inline def innerPush(e: JsonEvent) = {
				e match {
					case se: JsonStackElem => contextStack ::= se
					case sp: JsonStackPop => contextStack = contextStack.tail
					case _ => ()
				}
				out.push(e)
			}

			val signal = contextStack.headOption match {
				case Some(ArrayStart()) if !in.isArrayEnd =>
					// If an array just started, any event but ArrayEnd should push an Index(0) context.
					val s = IndexStart(0, in.location)
					innerPush(s) || innerPush(in)

				case Some(IndexStart(index)) if !in.isArrayEnd =>
					// If we're in the middle of an array, any event but ArrayEnd should advance the index.
					val e = IndexEnd(index, lastEventLocation)
					val c = IndexStart(index + 1, in.location)
					innerPush(e) || innerPush(c) || innerPush(in)

				case Some(IndexStart(index)) if in.isArrayEnd =>
					// If we're inside an array index when the array ends, inject an IndexEnd first.
					val e = IndexEnd(index, lastEventLocation)
					innerPush(e) || innerPush(in)

				case Some(FieldStart(name)) if in.asFieldStart.isDefined =>
					// If we encounter a new field while already in a field, inject a FieldEnd first
					val e = FieldEnd(name, lastEventLocation)
					innerPush(e) || innerPush(in)

				case Some(FieldStart(name)) if in.isObjectEnd =>
					// If the object ends while we're in a field, inject a FieldEnd first
					val e = FieldEnd(name, lastEventLocation)
					innerPush(e) || innerPush(in)

				case _ =>
					// All other events can be passed through normally
					innerPush(in)
			}

			lastEventLocation = in.location

			signal
		}

		def finish(out: Transformer.HandlerWrite[JsonEvent]) = {
			// in theory the stack should be empty if the JSON terminates naturally,
			// but if for some reason it doesn't, we'll inject "opposite" events to close out whatever stack state remains
			contextStack.foreach {
				case (_: ArrayStart) => out push ArrayEnd(ContextLocation.empty)
				case (_: ObjectStart) => out push ObjectEnd(ContextLocation.empty)
				case FieldStart(name) => out push FieldEnd(name, ContextLocation.empty)
				case IndexStart(index) => out push IndexEnd(index, ContextLocation.empty)
			}
		}
	}
}
