package io.dylemma.spac.types

/** Generalization for types that can be interpreted as a "push" or "pop" to a stack.
  * For example, `XMLEvent` has `StartElement` and `EndElement` subclasses which can be
  * treated as "push" and "pop" respectively.
  */
trait Stackable[T] {
	def isPush(elem: T): Boolean
	def isPop(elem: T): Boolean

	type StackElem
	def asPush(elem: T): Option[StackElem]
}

object Stackable {
	type Aux[T, S] = Stackable[T] { type StackElem = S }
}
