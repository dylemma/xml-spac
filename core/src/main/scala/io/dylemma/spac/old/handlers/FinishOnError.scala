package io.dylemma.spac.old.handlers

import io.dylemma.spac.old.Handler

import scala.util.Failure

trait FinishOnError { self: Handler[_, _] with ManualFinish =>
	def handleError(error: Throwable) = {
		finishWith { throw error }
	}
}
