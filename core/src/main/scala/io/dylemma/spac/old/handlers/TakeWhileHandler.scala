package io.dylemma.spac.old.handlers

import io.dylemma.spac.old.Handler

class TakeWhileHandler[In, Out](p: In => Boolean, inner: Handler[In, Out]) extends AbstractTakeWhileHandler[In, Out](inner) {
	override def toString = s"TakeWhile($p) >> $inner"
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
