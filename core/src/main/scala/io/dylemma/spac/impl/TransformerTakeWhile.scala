package io.dylemma.spac
package impl

case class TransformerTakeWhile[In](f: In => Boolean) extends Transformer[In, In] {
	def newHandler: Transformer.Handler[In, In] = new Transformer.Handler[In, In] {
		private var isTaking = true

		def push(in: In, out: Transformer.HandlerWrite[In]) = {
			if (isTaking && !f(in)) {
				isTaking = false
			}
			if (isTaking) out.push(in)
			else Signal.Stop
		}

		def finish(out: Transformer.HandlerWrite[In]): Unit = ()
	}
}
