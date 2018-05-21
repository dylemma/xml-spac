package io.dylemma.spac.json

import io.dylemma.spac.ContextMatcher
import io.dylemma.spac.handlers.{ContextMove, ContextTracker}

import scala.collection.mutable.ArrayBuffer


class JsonContextTracker extends ContextTracker[JsonEvent, JsonStackElem] {
	private var stack = new ArrayBuffer[JsonStackElem]

	def copyStack = stack.toList

	def currentDepth: Int = stack.length
	def checkContext[A](matcher: ContextMatcher[JsonStackElem, A]): Option[A] = matcher(stack, 0, stack.length)

	def evolve(event: JsonEvent) = {
		// if the event occurs inside an array, advance the index
		val top = if(stack.isEmpty) None else Some(stack(stack.length - 1)) // stack.last is O(N)

		/* Certain events need to be interpreted as multiple context moves.
		 * We handle them here by constructing a "continuation" that allows
		 * us to bounce control between this state tracker and another object
		 * that reacts to its `ContextMove`s.
		 */
		val prefixMoves: (() => ContextMove) => ContextMove = top match {
			// The array had just started previously, so now we're at index 0
			case Some(JsonStackElem.Array) if event != JsonEvent.ArrayEnd =>
				stack += JsonStackElem.Index(0)
				ContextMove.Multi(ContextMove.Push, _)

			// We were already inside the array, so this is the next index
			case Some(JsonStackElem.Index(i)) if event != JsonEvent.ArrayEnd =>
				// pop the current index
				stack.remove(stack.length - 1)
				finalMove: (() => ContextMove) => {
					ContextMove.Multi(ContextMove.Pop, () => {
						// push the new index
						stack += JsonStackElem.Index(i + 1)
						ContextMove.Multi(ContextMove.Push, finalMove)
					})
				}

			case Some(JsonStackElem.Index(_)) if event == JsonEvent.ArrayEnd =>
				stack.remove(stack.length - 1)
				ContextMove.Multi(ContextMove.Pop, _)

			// If we encounter another field while already in a field, pop the current field off so the new one can be added after
			case Some(JsonStackElem.Field(_)) if event.isInstanceOf[JsonEvent.ObjectField] =>
				stack.remove(stack.length - 1)
				ContextMove.Multi(ContextMove.Pop, _)

			// If the object ends while we're in a field, pop the current field off so the object can end cleanly
			case Some(JsonStackElem.Field(_)) if event == JsonEvent.ObjectEnd =>
				stack.remove(stack.length - 1)
				ContextMove.Multi(ContextMove.Pop, _)

			// everything else can be handled normally
			case _ =>
				finalMove: (() => ContextMove) => finalMove()
		}

		prefixMoves{ () => event match {
			case JsonEvent.ObjectStart =>
				stack += JsonStackElem.Object
				ContextMove.pushThenPass

			case JsonEvent.ObjectField(name) =>
				stack += JsonStackElem.Field(name)
				ContextMove.passThenPush

			case JsonEvent.ObjectEnd =>
				stack.remove(stack.length - 1)
				ContextMove.passThenPop

			case JsonEvent.ArrayStart =>
				stack += JsonStackElem.Array
				ContextMove.pushThenPass

			case JsonEvent.ArrayEnd =>
				stack.remove(stack.length - 1)
				ContextMove.passThenPop

			case e: JsonEvent.JsonValueEvent =>
				ContextMove.Pass

			case JsonEvent.Unknown =>
				ContextMove.Pass
		}}
	}
}
