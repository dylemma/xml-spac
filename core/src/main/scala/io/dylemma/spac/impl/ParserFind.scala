package io.dylemma.spac
package impl

import cats.Applicative

class ParserFind[F[+_], In](predicate: In => Boolean)(implicit F: Applicative[F]) extends Parser[F, In, Option[In]] {
	def step(_in: In) = F.map(F.pure(_in)) { in =>
		if (predicate(in)) Left(Some(in))
		else Right(this)
	}
	def finish = F.pure(None)
}