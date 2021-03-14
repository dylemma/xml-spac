package io.dylemma.spac
package impl

import cats.Applicative

class ParseFirstOpt[F[+_], In](implicit F: Applicative[F]) extends Parser[F, In, Option[In]] {
	def step(in: In): F[Either[Option[In], Parser[F, In, Option[In]]]] = F.pure(Left(Some(in)))
	def finish: F[Option[In]] = F.pure(None)
}
