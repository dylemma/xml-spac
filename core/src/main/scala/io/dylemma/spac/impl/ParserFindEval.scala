package io.dylemma.spac
package impl

import cats.Monad

class ParserFindEval[F[+_], In](predicate: In => F[Boolean])(implicit F: Monad[F]) extends Parser[F, In, Option[In]] {
	def step(in: In) = {
		F.map(predicate(in)) {
			case true => Left(Some(in))
			case false => Right(this)
		}
	}
	def finish = F.pure(None)
}

