package io.dylemma.spac.handlers

import io.dylemma.spac.Handler

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

class SafeConsumerHandler[In, Out](inner: Handler[In, Out]) extends Handler[In, Try[Out]]{
	def isFinished: Boolean = inner.isFinished
	def handleEnd(): Try[Out] = Try{ inner.handleEnd() }
	def handleInput(input: In): Option[Try[Out]] = {
		try inner.handleInput(input) map {Success(_)}
		catch { case NonFatal(err) => Some(Failure(err)) }
	}
	def handleError(error: Throwable): Option[Try[Out]] = {
		try inner.handleError(error) map {Success(_)}
		catch { case NonFatal(err) => Some(Failure(err)) }
	}
}
