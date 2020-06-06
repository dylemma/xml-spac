package io.dylemma.spac.old.handlers

import io.dylemma.spac.old.Handler

/** Transformer handler that will ignore all inputs, instead passing the given `error` exactly once to the downstream handler.
  *
  * @param error The error to pass
  * @param downstream The downstream handler
  * @tparam In The input type
  * @tparam Out The output type
  */
class EndWithErrorTransformerHandler[In, Out](error: Throwable, downstream: Handler[Nothing, Out]) extends Handler[In, Out] {
	private var didEmit = false
	private def emitWith[T](result: => T) = try result finally didEmit = true

	def isFinished: Boolean = didEmit || downstream.isFinished
	def handleInput(input: In) = {
		if(didEmit) None
		else emitWith{ downstream.handleError(error) }
	}
	def handleError(otherError: Throwable) = {
		if(didEmit) None
		else emitWith { downstream.handleError(otherError) }
	}
	def handleEnd() = {
		emitWith {
			val handledError = if (didEmit) None else downstream.handleError(error)
			handledError getOrElse downstream.handleEnd()
		}
	}
}
