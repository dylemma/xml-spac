package io.dylemma.spac.handlers

import io.dylemma.spac.ContextMatcher
import io.dylemma.spac.types.Stackable

import scala.collection.mutable.ArrayBuffer

/** A reaction to an event that can update some stack-like context.
  * Used by `ContextTracker` to indicate when/how its context stack has
  * changed, and where the event should fall relative to that change.
  *
  * Note that a well-behaved `ContextTracker` should always return
  * either a `Pass` or a `Multi` that will contain a `Pass` at some point.
  * This ensures that a Splitter relying on the ContextTracker will
  * actually handle the event.
  */
sealed trait ContextMove
object ContextMove {

	case object Push extends ContextMove
	case object Pop extends ContextMove
	case object Pass extends ContextMove
	case class Multi(head: ContextMove, tail: () => ContextMove) extends ContextMove

	val passThenPush = Multi(Pass, () => Push)
	val passThenPop = Multi(Pass, () => Pop)
	val pushThenPass = Multi(Push, () => Pass)
	val popThenPass = Multi(Pop, () => Pass)
}

trait ContextTracker[E, S] {
	def currentDepth: Int
	def copyStack: List[S]
	def checkContext[A](matcher: ContextMatcher[S, A]): Option[A]
	def evolve(event: E): ContextMove
}
