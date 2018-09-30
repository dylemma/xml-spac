package io.dylemma.spac.handlers

import io.dylemma.spac.Handler

/** Handler that multiplexes inputs to both a `downstream` and an `interrupter` handler.
  * If the `interrupter` handler returns an output (i.e. if it finishes), we send an EOF
  * signal to the `downstream` handler.
  *
  * @param downstream
  * @param interrupter
  * @tparam In  The input type
  * @tparam Out The output type
  */
class InterrupterHandler[In, Out](
	downstream: Handler[In, Out],
	interrupter: Handler[In, Any]
) extends Handler[In, Out] {

	def isFinished = downstream.isFinished || interrupter.isFinished

	def handleInput(input: In) = {
		val interrupt =
			if(interrupter.isFinished) true
			else interrupter.handleInput(input).isDefined
		if(interrupt){
			Some(downstream.handleEnd())
		} else {
			downstream.handleInput(input)
		}
	}

	def handleError(error: Throwable) = {
		val interrupt =
			if(interrupter.isFinished) true
			else interrupter.handleError(error).isDefined
		if(interrupt){
			Some(downstream.handleEnd())
		} else {
			downstream.handleError(error)
		}
	}
	def handleEnd() = downstream.handleEnd()
}
