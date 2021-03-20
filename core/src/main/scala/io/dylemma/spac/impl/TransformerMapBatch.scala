package io.dylemma.spac
package impl

// should be slightly more efficient than `self.through(Transformer.op(_.flatMap(...)))`
class TransformerMapBatch[In, A, B](self: Transformer[In, A], f: Emit[A] => Emit[B]) extends Transformer[In, B] {
	def newHandler = new TransformerMapBatch.Handler(self.newHandler, f)
}

object TransformerMapBatch {
	class Handler[In, A, B](
		private var inner: Transformer.Handler[In, A],
		f: Emit[A] => Emit[B]
	) extends Transformer.Handler[In, B]
	{
		def step(in: In) = inner.step(in) match {
			case (emit, nextInnerOpt) =>
				f(emit) -> nextInnerOpt.map { cont =>
					inner = cont
					this
				}
		}
		def finish() = f(inner.finish())
	}
}