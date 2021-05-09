package io.dylemma.spac
package impl

class TransformerMerge[In, Out](leftTransformer: Transformer[In, Out], rightTransformer: Transformer[In, Out]) extends Transformer[In, Out] {
	def newHandler: Transformer.Handler[In, Out] = new Transformer.Handler[In, Out] {
		val left = Transformer.Handler.protect(leftTransformer.newHandler)
		val right = Transformer.Handler.protect(rightTransformer.newHandler)

		def push(in: In, out: Transformer.HandlerWrite[Out]): Signal = {
			left.push(in, out) && right.push(in, out)
		}

		def finish(out: Transformer.HandlerWrite[Out]): Unit = {
			left.finish(out)
			right.finish(out)
		}
	}
}

object TransformerMerge {
	def apply[In, Out](left: Transformer[In, Out], right: Transformer[In, Out]) = {
		new TransformerMerge[In, Out](left, right)
	}
	def either[In, A, B](left: Transformer[In, A], right: Transformer[In, B]) = {
		new TransformerMerge[In, Either[A, B]](left.map(Left(_)), right.map(Right(_)))
	}

}