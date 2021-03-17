package io.dylemma.spac
package impl

import cats.Applicative

class TransformerTakeWhile[F[+_], In](f: In => Boolean)(implicit F: Applicative[F]) extends Transformer[F, In, In] {
	def step(in: In): F[(Emit[In], Option[Transformer[F, In, In]])] = {
		if (f(in)) F.pure { Emit.one(in) -> Some(this) }
		else F.pure { Emit.nil -> None }
	}
	def finish: F[Emit[In]] = F.pure(Emit.nil)
}
