package io.dylemma.spac.impl

import io.dylemma.spac.Transformer

case class TransformerScan[In, Out](init: Out, op: (Out, In) => Out) extends Transformer[In, Out] {
	def newHandler = new Transformer.Handler[In, Out] {
		private var state = init
		def push(in: In, out: Transformer.HandlerWrite[Out]) = {
			state = op(state, in)
			out.push(state)
		}
		def finish(out: Transformer.HandlerWrite[Out]): Unit = ()
	}
}
