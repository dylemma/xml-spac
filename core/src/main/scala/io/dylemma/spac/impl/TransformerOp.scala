package io.dylemma.spac
package impl

import cats.Applicative

class TransformerOp[F[+_], In, Out](op: In => Emit[Out])(implicit F: Applicative[F]) extends Transformer[F, In, Out] {
	def step(in: In): F[(Emit[Out], Option[Transformer[F, In, Out]])] = F.map(F.pure(in))(op(_) -> Some(this))
	def finish: F[Emit[Out]] = F.pure(Emit.nil)
}
