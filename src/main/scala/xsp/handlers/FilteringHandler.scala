package xsp.handlers

import xsp.Handler

class FilteringHandler[In, Out](p: In => Boolean, inner: Handler[In, Out]) extends AbstractHandler[In, Out](inner) {
	override def handleInput(input: In): Option[Out] = {
		if(isFinished) None
		else if(p(input)) inner.handleInput(input)
		else None
	}

	override def toString = s"Filter($p) >> $inner"
}
