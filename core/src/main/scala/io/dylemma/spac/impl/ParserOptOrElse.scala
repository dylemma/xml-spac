package io.dylemma.spac
package impl

import cats.Monad

class ParserOptOrElse[F[+_], -In, +Out](parser: Parser[F, In, Option[Out]], resultIfNone: F[Out])(implicit F: Monad[F]) extends Parser[F, In, Out] {
	def step(in: In): F[Either[Out, Parser[F, In, Out]]] = F.flatMap(parser.step(in)) {
		case Left(Some(out)) => F.pure(Left(out))
		case Left(None) => F.map(resultIfNone)(Left(_))
		case Right(`parser`) => F.pure(Right(this))
		case Right(nextParser) => F.pure(Right(new ParserOptOrElse(nextParser, resultIfNone)))
	}
	def finish: F[Out] = F.flatMap(parser.finish) {
		case Some(out) => F.pure(out)
		case None => resultIfNone
	}
}
