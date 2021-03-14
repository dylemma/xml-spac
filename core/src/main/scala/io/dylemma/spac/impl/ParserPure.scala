package io.dylemma.spac
package impl

import cats.Applicative

class ParserPure[F[+_], Out](value: Out)(implicit F: Applicative[F]) extends Parser[F, Any, Out] {
	def step(in: Any): F[Either[Out, Parser[F, Any, Out]]] = F.pure(Left(value))
	def finish: F[Out] = F.pure(value)
	override def toString = s"Parser.pure($value)"
}
