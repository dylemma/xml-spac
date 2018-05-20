package io.dylemma.spac.handlers

import io.dylemma.spac.ContextMatcher
import io.dylemma.spac.types.Stackable

import scala.collection.mutable.ArrayBuffer

sealed trait ContextMove
object ContextMove {
	case object Pop extends ContextMove
	case object Push extends ContextMove
	case object Noop extends ContextMove
}

class StackContextTracker[E, S](implicit stackable: Stackable.Aux[E, S]) {
	private var stack = new ArrayBuffer[S](10)

	def currentDepth: Int = stack.length
	def checkContext[A](matcher: ContextMatcher[S, A]): Option[A] = matcher(stack, 0, stack.length)

	def evolve(event: E): ContextMove = {
		stackable.asPush(event) match {
			case Some(stackElem) =>
				stack += stackElem
				ContextMove.Push
			case None =>
				if(stackable isPop event){
					stack.remove(stack.length - 1)
					ContextMove.Pop
				} else {
					ContextMove.Noop
				}
		}
	}
}
