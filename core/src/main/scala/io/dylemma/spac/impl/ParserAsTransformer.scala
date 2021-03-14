package io.dylemma.spac
package impl

import cats.Functor
import cats.data.Chain

class ParserAsTransformer[F[+_], -In, +Out](parser: Parser[F, In, Out])(implicit F: Functor[F]) extends Transformer[F, In, Out] {
	def step(in: In) = F.map(parser.step(in)) {
		case Left(out) => Chain.one(out) -> None
		case Right(nextParser) =>
			val nextTransform = if (nextParser eq parser) this else new ParserAsTransformer(nextParser)
			Chain.nil -> Some(nextTransform)
	}
	def finish = F.map(parser.finish) { Chain.one }
}