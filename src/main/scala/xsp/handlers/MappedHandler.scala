package xsp.handlers

import xsp.Handler

class MappedHandler[In, A, B](inner: Handler[In, A], f: A => B) extends Handler[In, B] {
	def isFinished: Boolean = inner.isFinished
	def handleEnd(): B = f(inner.handleEnd())
	def handleInput(input: In): Option[B] = inner.handleInput(input).map(f)
	def handleError(error: Throwable): Option[B] = inner.handleError(error).map(f)
}
