package io.dylemma.xsp.handlers

import io.dylemma.xsp.Handler

class MappedHandler[In, A, B](f: A => B, inner: Handler[In, A]) extends Handler[In, B] {
	override def toString = s"Map($f) >> $inner"

	def isFinished: Boolean = inner.isFinished
	def handleEnd(): B = f(inner.handleEnd())
	def handleInput(input: In): Option[B] = inner.handleInput(input).map(f)
	def handleError(error: Throwable): Option[B] = inner.handleError(error).map(f)
}
