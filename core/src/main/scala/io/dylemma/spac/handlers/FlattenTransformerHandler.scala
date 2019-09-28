package io.dylemma.spac.handlers

import io.dylemma.spac.Handler

import scala.annotation.tailrec

class FlattenTransformerHandler[A, Out](downstream: Handler[A, Out]) extends Handler[Iterable[A], Out] {
	def isFinished: Boolean = downstream.isFinished
	def handleError(error: Throwable): Option[Out] = downstream.handleError(error)
	def handleEnd(): Out = downstream.handleEnd()

	def handleInput(input: Iterable[A]): Option[Out] = {
		val itr = input.iterator

		// consume items from the iterator, feeding them to `downstream.handleInput` until either
		// we fully-consume the iterator or the downstream produces a result
		@tailrec def run: Option[Out] = {
			if(itr.hasNext) {
				if(downstream.isFinished) {
					// semi-illegal state; handleInput shouldn't have been called, but we'll return None
					None
				} else {
					downstream.handleInput(itr.next) match {
						// downstream is ready for more
						case None =>
							run

						// downstream produced a result and we can stop the iterator early
						case out => out
					}
				}
			} else {
				// done consuming the iterator but didn't get a result from the downstream
				None
			}
		}
		run
	}
}
