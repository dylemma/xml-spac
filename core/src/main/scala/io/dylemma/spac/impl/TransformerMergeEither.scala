package io.dylemma.spac
package impl

class TransformerMergeEither[In, A, B](left: Transformer[In, A], right: Transformer[In, B]) extends Transformer[In, Either[A, B]] {
	def newHandler = new TransformerMergeEither.Handler(left.newHandler, right.newHandler)
}

object TransformerMergeEither {
	class Handler[In, A, B](
		private var left: Transformer.Handler[In, A],
		private var right: Transformer.Handler[In, B]
	) extends Transformer.Handler[In, Either[A, B]] {
		def step(in: In) = {
			val (emitA, contA) = left.step(in)
			val (emitB, contB) = right.step(in)
			val emit: Emit[Either[A, B]] = emitA.map(Left(_)) ++ emitB.map(Right(_))
			val cont = (contA, contB) match {
				case (Some(l), Some(r)) =>
					left = l
					right = r
					Some(this)
				case (Some(l), None) =>
					Some { new TransformerMapBatch.Handler[In, A, Either[A, B]](l, _.map(Left(_))) }
				case (None, Some(r)) =>
					Some { new TransformerMapBatch.Handler[In, B, Either[A, B]](r, _.map(Right(_))) }
				case (None, None) =>
					None
			}
			emit -> cont
		}

		def finish() = {
			left.finish().map(Left(_)) ++ right.finish().map(Right(_))
		}
	}
}