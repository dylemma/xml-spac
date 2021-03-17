package io.dylemma.spac
package impl

import cats.Functor

class TransformerMapBatch[F[+_], -In, Out, +Out2](inner: Transformer[F, In, Out], f: Emit[Out] => Emit[Out2])(implicit F: Functor[F]) extends Transformer[F, In, Out2] {
	def step(in: In): F[(Emit[Out2], Option[Transformer[F, In, Out2]])] = F.map(inner.step(in)) {
		case (emit, nextInnerOpt) =>
			val nextEmit = f(emit)
			val nextTransformer = nextInnerOpt.map {
				case `inner` => this // "pure" transformers don't require extra allocations since there's no state change
				case nextInner => new TransformerMapBatch(nextInner, f)
			}
			nextEmit -> nextTransformer
	}
	def finish: F[Emit[Out2]] = F.map(inner.finish)(f)
}