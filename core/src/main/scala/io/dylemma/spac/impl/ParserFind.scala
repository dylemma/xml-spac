package io.dylemma.spac
package impl

import cats.Applicative

class ParserFind[F[+_], In](predicate: In => Boolean)(implicit F: Applicative[F]) extends Parser[F, In, Option[In]] {
	def step(in: In) = {
		if (predicate(in)) F.pure { Left(Some(in)) }
		else F.pure { Right(this) }
	}
	def finish = F.pure(None)
}