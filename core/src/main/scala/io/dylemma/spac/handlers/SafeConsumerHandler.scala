package io.dylemma.spac.handlers

import io.dylemma.spac.Handler

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/** Wraps an `inner` handler's methods with a `try`, wrapping the output type with `Try[_]`
  */
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

/** Unwraps an `inner` handler whose output type is a `Try`, so that any `Failure` returned from
  * the inner handler will have its underlying error thrown.
  */
class UnwrapSafeConsumerHandler[In, Out](inner: Handler[In, Try[Out]]) extends Handler[In, Out]{
	def isFinished = inner.isFinished
	def handleInput(input: In) = inner.handleInput(input).map(_.get)
	def handleError(error: Throwable) = inner.handleError(error).map(_.get)
	def handleEnd() = inner.handleEnd.get
}