package io.dylemma.spac.handlers

import io.dylemma.spac.Handler

class FilteringHandler[In, Out](p: In => Boolean, val downstream: Handler[In, Out]) extends TransformerHandler[In, In, Out] {
	override def toString = s"Filter($p) >> $downstream"
	protected def transformInput(input: In): Option[In] = if(p(input)) Some(input) else None
}
