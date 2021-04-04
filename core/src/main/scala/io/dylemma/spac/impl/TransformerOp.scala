package io.dylemma.spac
package impl

class TransformerOp[In, Out](op: In => Emit[Out]) extends Transformer.Stateless[In, Out] {
	def step(in: In) = op(in) -> Some(this)
	def finish() = Emit.nil
	override def toString = "Transformer.op(..)"
}
