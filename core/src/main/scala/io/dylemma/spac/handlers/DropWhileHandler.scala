package io.dylemma.spac.handlers

import io.dylemma.spac.Handler

class DropWhileHandler[In, Out](p: In => Boolean, val downstream: Handler[In, Out]) extends TransformerHandler[In, In, Out]{
	override def toString = s"DropWhile($p) >> $downstream"
	private var isDropping = true

	protected def transformInput(input: In): Option[In] = {
		if(isDropping && p(input)) None
		else {
			isDropping = false
			Some(input)
		}
	}
}
