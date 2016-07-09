package io.dylemma.xsp.handlers

import io.dylemma.xsp.Handler

class DropNHandler[In, Out](n: Int, inner: Handler[In, Out]) extends AbstractHandler[In, Out](inner) {
	override def toString = s"Take($n) >> $inner"
	private var droppedCount = 0
	override def handleInput(input: In): Option[Out] = {
		if(isFinished) None
		else {
			val shouldSkip = droppedCount < n
			droppedCount += 1
			if (shouldSkip) None
			else inner.handleInput(input)
		}
	}
}
