package io.dylemma.spac
package impl

import cats.Applicative

class ParserFold[F[+_], In, Out](state: Out, fold: (Out, In) => Out)(implicit F: Applicative[F]) extends Parser[F, In, Out] {
	def step(in: In): F[Either[Out, Parser[F, In, Out]]] = F.map(F.pure(in)) { x => Right(new ParserFold(fold(state, x), fold)) }
	def finish: F[Out] = F.pure(state)
}