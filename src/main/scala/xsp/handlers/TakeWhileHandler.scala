package xsp.handlers

import xsp.Handler

class TakeWhileHandler[In, Out](p: In => Boolean, inner: Handler[In, Out]) extends AbstractTakeWhileHandler[In, Out](inner) {
	private var conditionBroken = false
	protected def shouldBeFinished: Boolean = conditionBroken
	protected def feedInput(input: In): Option[Out] = {
		val conditionMet = p(input)
		if(conditionMet) inner.handleInput(input)
		else {
			conditionBroken = true
			None
		}
	}
}
