package io.dylemma
package spac

class TransformerScan[In, Out](init: Out, op: (Out, In) => Out) extends Transformer[In, Out] {
	def newHandler = new TransformerScan.Handler(init, op)
}

object TransformerScan {
	class Handler[In, Out](private var state: Out, op: (Out, In) => Out) extends Transformer.Handler[In, Out] {
		def step(in: In) = {
			state = op(state, in)
			Emit.one(state) -> Some(this)
		}
		def finish() = Emit.nil
	}
}
