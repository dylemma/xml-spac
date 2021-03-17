package io.dylemma.spac
package impl

import cats.Monad
import cats.syntax.all._

class TransformerInto[F[+_] : Monad, -In, X, Out](transformer: Transformer[F, In, X], parser: Parser[F, X, Out]) extends Parser[F, In, Out] {
	def step(in: In): F[Either[Out, Parser[F, In, Out]]] = transformer.step(in) flatMap {
		case (xEmit, None) =>
			// transformer has finished, so after the `xEmit` that's the EOF so the parser may need to be finished
			parser.stepMany(xEmit) flatMap {
				// parser finished on its own
				case Left((out, leftovers)) => Left(out).pure[F]
				// parser needs to be explicitly finished
				case Right(finalParser) => finalParser.finish.map(Left(_))
			}

		case (xEmit, Some(nextTransformer)) =>
			// transformer is emitting some values before continuing with a new state
			parser.stepMany(xEmit) map {
				// parser finished
				case Left((out, leftovers)) => Left(out)
				// parser didn't finish, so we can continue from its new state
				case Right(nextParser) => Right(new TransformerInto(nextTransformer, nextParser))
			}
	}
	def finish: F[Out] = transformer.finish.flatMap { xEmit =>
		parser.stepMany(xEmit) flatMap {
			case Left((out, leftovers)) => out.pure[F]
			case Right(finalParser) => finalParser.finish
		}
	}
}
