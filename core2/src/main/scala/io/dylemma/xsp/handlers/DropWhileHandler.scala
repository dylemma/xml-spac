package io.dylemma.xsp.handlers

import io.dylemma.xsp.Handler

class DropWhileHandler[In, Out](p: In => Boolean, inner: Handler[In, Out]) extends AbstractHandler[In, Out](inner){
	override def toString = s"DropWhile($p) >> $inner"
	private var isDropping = true
	override def handleInput(input: In): Option[Out] = {
		if(isFinished) None
		else{
			if(isDropping && p(input)) None
			else {
				isDropping = false
				inner.handleInput(input)
			}
		}
	}
}
