package io.dylemma.spac
package impl

case class TransformerDropWhile[In](f: In => Boolean) extends Transformer[In, In] {
	def newHandler: Transformer.Handler[In, In] = new TransformerDropWhile.Handler(f)
}
object TransformerDropWhile {
	class Handler[In](f: In => Boolean) extends Transformer.Handler[In, In] {
		private var isDropping = true
		def push(in: In, out: Transformer.HandlerWrite[In]) = {
			// test whether to stop dropping
			if (isDropping && !f(in)) {
				isDropping = false
			}
			if (isDropping) Signal.Continue
			else out.push(in)
		}
		def finish(out: Transformer.HandlerWrite[In]) = ()
	}
}
