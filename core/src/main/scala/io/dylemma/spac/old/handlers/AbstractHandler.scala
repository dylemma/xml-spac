package io.dylemma.spac.old.handlers

import io.dylemma.spac.old.Handler

abstract class AbstractHandler[In, Out](protected val inner: Handler[In, Out]) extends Handler[In, Out] {
	def isFinished = inner.isFinished
	def handleInput(input: In): Option[Out] = {
		if(isFinished) None
		else inner handleInput input
	}
	def handleError(err: Throwable): Option[Out] = {
		if(isFinished) None
		else inner handleError err
	}
	def handleEnd(): Out = {
		if(isFinished) throw new IllegalStateException("handleEnd() called when isFinished is true")
		inner.handleEnd()
	}
}
