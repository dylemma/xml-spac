package io.dylemma.xsp.handlers

import io.dylemma.xsp.Handler

class TakeNHandler[In, Out](max: Int, inner: Handler[In, Out]) extends AbstractTakeWhileHandler[In, Out](inner) {
	override def toString = s"Take($max) >> $inner"
	private var counter = 0
	protected def shouldBeFinished: Boolean = counter >= max
	protected def feedInput(input: In): Option[Out] = {
		counter += 1
		inner.handleInput(input)
	}
}
