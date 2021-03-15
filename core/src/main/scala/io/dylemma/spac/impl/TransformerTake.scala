package io.dylemma.spac
package impl

import cats.Applicative

class TransformerTake[F[+_], In](remaining: Int)(implicit F: Applicative[F]) extends Transformer[F, In, In] {
	def step(in: In): F[(Emit[In], Option[Transformer[F, In, In]])] = {
		if (remaining > 1) F.pure { Emit.one(in) -> Some(new TransformerTake(remaining - 1)) }
		else if (remaining == 1) F.pure { Emit.one(in) -> None }
		else F.pure { Emit.nil -> None }
	}
	def finish: F[Emit[In]] = F.pure(Emit.nil)
}
