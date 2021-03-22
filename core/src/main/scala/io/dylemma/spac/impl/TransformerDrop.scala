package io.dylemma.spac
package impl

class TransformerDrop[In](n: Int) extends Transformer[In, In] {
	def newHandler = new TransformerDrop.Handler(n)
}

object TransformerDrop {
	class Handler[In](private var remaining: Int) extends Transformer.Handler[In, In] {
		def step(in: In) = {
			if (remaining <= 0) {
				Emit.one(in) -> Some(Transformer.identity.newHandler)
			} else {
				remaining -= 1
				Emit.nil -> Some(this)
			}
		}
		def finish() = Emit.nil
	}
}
