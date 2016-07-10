package io.dylemma.spac.handlers

import io.dylemma.spac.Handler

class CollectHandler[A, B, Out](pf: PartialFunction[A, B], val downstream: Handler[B, Out]) extends TransformerHandler[A, B, Out] {
	override def toString = s"Collect($pf) >> $downstream"
	protected def transformInput(input: A): Option[B] = {
		if(pf isDefinedAt input) Some(pf(input)) else None
	}
}
