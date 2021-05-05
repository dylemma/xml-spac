package io.dylemma.spac
package impl

case class TransformerTake[In](n: Int) extends Transformer[In, In] {
	def newHandler = new Transformer.Handler[In, In] {
		private var remaining = n
		def push(in: In, out: Transformer.HandlerWrite[In]) = {
			if (remaining > 1) {
				remaining -= 1
				out.push(in)
			} else if (remaining == 1) {
				remaining = 0
				out.push(in)
				Signal.Stop
			} else {
				Signal.Stop
			}
		}
		def finish(out: Transformer.HandlerWrite[In]): Unit = ()
	}
}