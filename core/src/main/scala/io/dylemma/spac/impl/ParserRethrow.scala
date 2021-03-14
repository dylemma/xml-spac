package io.dylemma.spac
package impl

import cats.MonadError

class ParserRethrow[F[+_], In, Out, Err](p: Parser[F, In, Either[Err, Out]])(implicit F: MonadError[F, Err]) extends Parser[F, In, Out] {
	def step(in: In): F[Either[Out, Parser[F, In, Out]]] = F.flatMap(p.step(in)) {
		case Right(cont) => F pure Right(new ParserRethrow(cont))
		case Left(Left(err)) => F raiseError err
		case Left(Right(result)) => F pure Left(result)
	}
	def finish: F[Out] = F.rethrow(p.finish)
}
