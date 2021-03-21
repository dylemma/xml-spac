package io.dylemma.spac
package impl

class TransformerTap[In](f: In => Unit) extends Transformer.Stateless[In, In] {
	def step(in: In) = {
		f(in)
		Emit.one(in) -> Some(this)
	}
	def finish() = Emit.nil
}
