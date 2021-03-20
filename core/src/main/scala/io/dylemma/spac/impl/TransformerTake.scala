package io.dylemma.spac
package impl

class TransformerTake[In](n: Int) extends Transformer[In, In] {
	def newHandler = new TransformerTake.Handler(n)
}

object TransformerTake {
	class Handler[In](private var remaining: Int) extends Transformer.Handler[In, In] {
		def step(in: In) = {
			if (remaining > 1) {
				remaining -= 1
				Emit.one(in) -> Some(this)
			} else if (remaining == 1) {
				Emit.one(in) -> None
			} else {
				Emit.nil -> None
			}
		}
		def finish() = Emit.nil
	}
}