package xsp.handlers

import xsp.{Handler, Result}

/** Handler that delegates to an `inner` handler, converting result-typed inputs
	* to the corresponding `handleInput` or `handleError` call.
	*/
class ResultFlatteningHandler[A, B](inner: Handler[A, B]) extends Handler[Result[A], B] {
	def isFinished: Boolean = inner.isFinished
	def handleEnd(): B = inner.handleEnd()
	def handleInput(input: Result[A]): Option[B] = input match {
		case Result.Success(a) => inner.handleInput(a)
		case Result.Error(e) => inner.handleError(e)
		case Result.Empty => None
	}
	def handleError(error: Throwable): Option[B] = inner.handleError(error)
}

/** Handler that upgrades events from the upstream to `Results` before passing them
	* to the inner handler. This way, the inner handler will never have its `handleError`
	* method called.
	*/
class ResultExpandingHandler[A, B](inner: Handler[Result[A], B]) extends Handler[A, B] {
	def isFinished = inner.isFinished
	def handleEnd = inner.handleEnd()
	def handleInput(input: A) = inner.handleInput(Result.Success(input))
	def handleError(err: Throwable) = inner.handleInput(Result.Error(err))
}