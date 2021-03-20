package io.dylemma.spac
package impl

class TransformerTakeWhile[In](f: In => Boolean) extends Transformer.Stateless[In, In] {
	def step(in: In) = {
		if (f(in)) Emit.one(in) -> Some(this)
		else Emit.nil -> None
	}
	def finish() = Emit.nil
}
