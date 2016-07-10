package io.dylemma.spac.handlers

import io.dylemma.spac.Handler

import scala.util.Failure

trait FinishOnError { self: Handler[_, _] with ManualFinish =>
	def handleError(error: Throwable) = {
		finishWith(Some(Failure(error)))
	}
}
