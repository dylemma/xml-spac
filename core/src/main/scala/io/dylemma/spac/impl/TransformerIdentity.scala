package io.dylemma.spac
package impl

class TransformerIdentity[In] extends Transformer.Stateless[In, In] {
	def step(in: In) = Emit.one(in) -> Some(this)
	def finish() = Emit.empty
}
