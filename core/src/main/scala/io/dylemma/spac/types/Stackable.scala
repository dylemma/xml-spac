package io.dylemma.spac.types

/** Generalization for event types that can be interpreted as a "push" or "pop" to a stack.
  * For example, `XMLEvent` has `StartElement` and `EndElement` subclasses which can be
  * treated as "push" and "pop" respectively.
  */
trait Stackable[-E] {

	/** The type of elements in a context stack that will be affected by events. */
	type StackElem

	/** Test if the event is one that will cause a "push" to the context stack.
	  *
	  * @param event An event
	  * @return `true` if the event would cause a "push" to the context stack.
	  */
	def isPush(event: E): Boolean

	/** Test if the event is one that will cause a "pop" from the context stack.
	  *
	  * @param event An event
	  * @return `true` if the event would cause a "pop" from the context stack.
	  */
	def isPop(event: E): Boolean

	/** Signals where a context change (if any) should happen relative to handling the event.
	  * This allows a splitter to decide to include or exclude the stack-changing event in
	  * its context.
	  *
	  * Returning a negative number signifies the context change should happen *before* handling
	  * the event, which would cause the event to become part of the new, changed context.
	  *
	  * Returning a positive number (or 0) signifies the context change should happen *after*
	  * handling the event, which would cause the event to be excluded from the new context.
	  *
	  * Returning 0 should be reserved for events that are neither a pop nor a push.
	  *
	  * @param event
	  * @return
	  */
	def order(event: E): Int

	/** Convert an event to its optional corresponding stack value.
	  *
	  * @param event An event
	  * @return `None` if `isPush(event) == false`, otherwise a `Some` with
	  *        the corresponding stack value
	  */
	def asPush(event: E): Option[StackElem]
}

object Stackable {
	type Aux[T, S] = Stackable[T] { type StackElem = S }
}
