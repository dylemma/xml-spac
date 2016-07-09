package io.dylemma.spac.handlers

import io.dylemma.spac.{Handler, Result}

import scala.util.control.NonFatal

class SafeConsumerHandler[In, Out](inner: Handler[In, Out]) extends Handler[In, Result[Out]]{
	def isFinished: Boolean = inner.isFinished
	def handleEnd(): Result[Out] = Result{ inner.handleEnd() }
	def handleInput(input: In): Option[Result[Out]] = {
		try inner.handleInput(input) map {Result.Success(_)}
		catch { case NonFatal(err) => Some(Result.Error(err)) }
	}
	def handleError(error: Throwable): Option[Result[Out]] = {
		try inner.handleError(error) map {Result.Success(_)}
		catch { case NonFatal(err) => Some(Result.Error(err)) }
	}
}
