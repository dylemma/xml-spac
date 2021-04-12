package io.dylemma.spac
package impl

class TransformerDropWhile[In](f: In => Boolean) extends Transformer.Stateless[In, In] {
	def step(in: In) = {
		if (f(in)) {
			// drop it
			Emit.nil -> Some(this)
		} else {
			// stop dropping
			Emit.one(in) -> Some(Transformer.identity.newHandler)
		}
	}
	def finish() = Emit.nil
}
