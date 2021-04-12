package io.dylemma.spac
package impl

class TransformerMerge[In, A, B, Out](left: Transformer[In, A], fa: A => Out, right: Transformer[In, B], fb: B => Out) extends Transformer[In, Out] {
	def newHandler = new TransformerMerge.Handler(left.newHandler, right.newHandler, fa, fb)
}

object TransformerMerge {
	def apply[In, Out](left: Transformer[In, Out], right: Transformer[In, Out]) = new TransformerMerge[In, Out, Out, Out](left, identity, right, identity)
	def either[In, A, B](left: Transformer[In, A], right: Transformer[In, B]) = new TransformerMerge[In, A, B, Either[A, B]](left, Left(_), right, Right(_))

	class Handler[In, A, B, Out](
		private var left: Transformer.Handler[In, A],
		private var right: Transformer.Handler[In, B],
		fa: A => Out,
		fb: B => Out,
	) extends Transformer.Handler[In, Out] {
		def step(in: In) = {
			val (emitA, contA) = left.step(in)
			val (emitB, contB) = right.step(in)
			val emit: Emit[Out] = emitA.map(fa) ++ emitB.map(fb)
			val cont = (contA, contB) match {
				case (Some(l), Some(r)) =>
					left = l
					right = r
					Some(this)
				case (Some(l), None) =>
					Some { new TransformerMapBatch.Handler[In, A, Out](l, _.map(fa)) }
				case (None, Some(r)) =>
					Some { new TransformerMapBatch.Handler[In, B, Out](r, _.map(fb)) }
				case (None, None) =>
					None
			}
			emit -> cont
		}

		def finish() = {
			left.finish().map(fa) ++ right.finish().map(fb)
		}
	}
}