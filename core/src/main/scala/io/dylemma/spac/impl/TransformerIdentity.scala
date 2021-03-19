package io.dylemma.spac
package impl

import cats.Applicative

class TransformerIdentity[F[+_], In](implicit F: Applicative[F]) extends Transformer[F, In, In] {
	def step(in: In): F[(Emit[In], Option[Transformer[F, In, In]])] = F.pure(Emit.one(in) -> Some(this))
	def finish: F[Emit[In]] = F.pure(Emit.nil)
}
