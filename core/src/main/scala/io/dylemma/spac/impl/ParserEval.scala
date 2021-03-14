package io.dylemma.spac
package impl

import cats.Monad

class ParserEval[F[+_], In, Out](init: F[Parser[F, In, Out]])(implicit F: Monad[F]) extends Parser[F, In, Out] {
	def step(in: In) = F.flatMap(init)(_ step in)
	def finish: F[Out] = F.flatMap(init)(_.finish)
}