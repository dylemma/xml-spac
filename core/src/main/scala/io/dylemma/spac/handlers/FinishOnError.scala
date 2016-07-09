package io.dylemma.spac.handlers

import io.dylemma.spac.{Handler, Result}

trait FinishOnError { self: Handler[_, _] with ManualFinish =>
	def handleError(error: Throwable) = {
		finishWith(Some(Result.Error(error)))
	}
}
