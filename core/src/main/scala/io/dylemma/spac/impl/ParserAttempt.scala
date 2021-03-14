package io.dylemma.spac
package impl

import cats.MonadError

class ParserAttempt[F[+_], In, Out, Err](p: Parser[F, In, Out])(implicit F: MonadError[F, Err]) extends Parser[F, In, Either[Err, Out]]{
	def step(in: In): F[Either[Either[Err, Out], Parser[F, In, Either[Err, Out]]]] = F.map(F.attempt(p step in)) {
		case Left(err) => Left(Left(err)) // stop parser with an error result
		case Right(Left(result)) => Left(Right(result)) // stop parser with a successful result
		case Right(Right(cont)) => Right(new ParserAttempt(cont)) // continue parsing with a new state
	}
	def finish: F[Either[Err, Out]] = F.attempt(p.finish)
}
