package io.dylemma.xsp.handlers

import io.dylemma.xsp.Handler

class DropNHandler[In, Out](n: Int, val downstream: Handler[In, Out]) extends TransformerHandler[In, In, Out] {
	override def toString = s"Take($n) >> $downstream"
	private var droppedCount = 0

	protected def transformInput(input: In): Option[In] = {
		val shouldSkip = droppedCount < n
		droppedCount += 1
		if (shouldSkip) None
		else Some(input)
	}
}
