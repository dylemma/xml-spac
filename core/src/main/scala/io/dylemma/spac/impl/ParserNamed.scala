package io.dylemma.spac
package impl

import cats.Functor

class ParserNamed[F[+_], In, Out](name: String, p: Parser[F, In, Out])(implicit F: Functor[F]) extends Parser[F, In, Out] {
	override def toString = name
	def step(in: In): F[Either[Out, Parser[F, In, Out]]] = F.map(p.step(in)) {
		case Left(out) => Left(out)
		case Right(`p`) => Right(this)
		case Right(cont) => Right(new ParserNamed(name, cont))
	}
	def finish: F[Out] = p.finish
}
