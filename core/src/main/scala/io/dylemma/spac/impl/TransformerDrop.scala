package io.dylemma.spac
package impl

case class TransformerDrop[In](n: Int) extends Transformer[In, In] {
	def newHandler = new TransformerDrop.Handler(n)
}

object TransformerDrop {
	class Handler[In](private var remaining: Int) extends Transformer.Handler[In, In] {
		def push(in: In, out: Transformer.HandlerWrite[In]): Signal = {
			if (remaining <= 0) out.push(in)
			else {
				remaining -= 1
				Signal.Continue
			}
		}
		def finish(out: Transformer.HandlerWrite[In]): Unit = ()
	}
}
