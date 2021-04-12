package io.dylemma.spac
package impl

import cats.data.Chain
import io.dylemma.spac.SpacTraceElement

class TransformerSpacFrame[A](elems: Chain[SpacTraceElement]) extends Transformer.Stateless[A, A] {
	def step(in: A) = Emit.one(in) -> Some(this)
	def finish() = Emit.nil
	override def unwind(err: Throwable) = SpacException.addTrace(err, elems)
}

