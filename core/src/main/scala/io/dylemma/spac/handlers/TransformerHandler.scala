package io.dylemma.spac.handlers

import io.dylemma.spac.Handler

import scala.util.{Failure, Success, Try}

/** Base implementation for Handlers that transform and/or filter
	* inputs before passing them along to some downstream handler.
	*
	* @tparam A   The input type
	* @tparam B   The transformed input type, which corresponds to the input type of the downstream handler
	* @tparam Out The downstream handler's output type
	*/
trait TransformerHandler[A, B, Out] extends Handler[A, Out] {
	protected def downstream: Handler[B, Out]
	protected def transformInput(input: A): Option[B]

	def isFinished = downstream.isFinished

	def handleInput(input: A) = {
		Try {transformInput(input)} match {
			case Success(bInput) => bInput flatMap downstream.handleInput
			case Failure(err) => downstream handleError err
		}
	}

	def handleError(err: Throwable) = downstream handleError err

	def handleEnd() = downstream.handleEnd()
}
