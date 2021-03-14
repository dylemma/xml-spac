package io.dylemma.spac
package impl

import cats.Monad

class ParserFoldEval[F[+_], In, Out](state: Out, fold: (Out, In) => F[Out])(implicit F: Monad[F]) extends Parser[F, In, Out] {
	def step(in: In) = F.map(F.flatMap(F.pure(in)) { fold(state, _) }) { nextState => Right(new ParserFoldEval(nextState, fold)) }
	def finish = F.pure(state)
}