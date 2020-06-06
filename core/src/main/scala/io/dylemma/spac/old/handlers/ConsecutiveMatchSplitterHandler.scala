package io.dylemma.spac.old.handlers

import io.dylemma.spac.old.{ContextSensitiveHandler, Handler}

import scala.util.{Failure, Success, Try}

/** Handler for `Splitter.consecutiveMatches`
  *
  * @param matcher
  * @param downstream
  * @tparam In  The input type
  * @tparam Context The context type
  * @tparam Out The output type
  */
class ConsecutiveMatchSplitterHandler[In, Context, Out](
	matcher: PartialFunction[In, Context],
	downstream: ContextSensitiveHandler[In, Context, Out]
) extends Handler[In, Out] {

	private var isInContext = false

	def isFinished = downstream.isFinished
	def handleError(error: Throwable) = downstream.handleError(error)
	def handleEnd() = downstream.handleEnd()

	def handleInput(input: In) = {
		Try { matcher.lift(input) } match {
			case Success(ctxMatch) =>
				ctxMatch match {
					// remaining in the same context
					case Some(_) if isInContext => downstream.handleInput(input)
					case None if !isInContext => downstream.handleInput(input)

					// entering a new context
					case Some(newCtx) if !isInContext =>
						isInContext = true
						downstream.handleContextStart(Success(newCtx)) orElse downstream.handleInput(input)

					// leaving the context
					case None if isInContext =>
						isInContext = false
						downstream.handleContextEnd() orElse downstream.handleInput(input)
				}
			case Failure(err) =>
				if(isInContext){
					// treat the failure as an error to be sent downstream
					downstream.handleError(err)
				} else {
					// otherwise treat it like a failed context start
					isInContext = true
					downstream.handleContextStart(Failure(err))
				}
		}
	}
}
