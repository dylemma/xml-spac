package io.dylemma.spac.old.handlers

import io.dylemma.spac.old.Handler
import io.dylemma.spac.old.{ContextSensitiveHandler, Handler}

import scala.util.{Failure, Success, Try}

class SplitOnMatchHandler[In, Context, Out](
	matcher: PartialFunction[In, Context],
	downstream: ContextSensitiveHandler[In, Context, Out]
) extends Handler[In, Out] {

	def isFinished = downstream.isFinished
	def handleError(error: Throwable) = downstream.handleError(error)
	def handleEnd() = downstream.handleEnd()

	private var startedMatching = false

	def handleInput(input: In) = {
		Try { matcher.lift(input) } match {
			case Success(ctxMatch) =>

				ctxMatch match {
					// forward the event since we're in a context
					case None if startedMatching => downstream.handleInput(input)

					// all inputs before the initial match are ignored
					case None => None

					// start a new context
					case Some(ctx) =>
						// replace the old one (if any)
						val resultFromEnd = if(startedMatching) downstream.handleContextEnd() else None
						// then start the new one and send the event
						resultFromEnd orElse {
							startedMatching = true
							downstream handleContextStart Success(ctx)
						} orElse {
							downstream handleInput input
						}
				}

			case Failure(err) =>
				startedMatching = true
				downstream handleContextStart Failure(err)
		}
	}
}
