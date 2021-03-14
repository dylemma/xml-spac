package io.dylemma.spac
package impl

import cats.Functor

class ParserMapped[F[+_], In, Out, Out2](inner: Parser[F, In, Out], f: Out => Out2)(implicit F: Functor[F]) extends Parser[F, In, Out2] {
	def step(in: In): F[Either[Out2, Parser[F, In, Out2]]] = F.map(inner.step(in)) {
		case Left(out) => Left(f(out))
		case Right(`inner`) => Right(this)
		case Right(nextInner) => Right(new ParserMapped(nextInner, f))
	}
	def finish: F[Out2] = F.map(inner.finish)(f)
}