package io.dylemma.spac
package impl

import cats.Monad
import cats.syntax.all._

class TransformerThrough[F[+_], -In, X, +Out](first: Transformer[F, In, X], second: Transformer[F, X, Out])(implicit F: Monad[F]) extends Transformer[F, In, Out] {
	def step(in: In): F[(Emit[Out], Option[Transformer[F, In, Out]])] = first.step(in) flatMap {
		case (xEmit, None) =>
			// first is finished, so feed its results to the `second` parser and then finish that one too
			second.stepMany(xEmit) flatMap {
				case (outs, Left(leftoverX)) => F.pure(outs -> None)
				case (outs, Right(continueSecond)) => continueSecond.finish map { finalOuts => (outs ++ finalOuts) -> None }
			}

		case (xEmit, Some(continueFirst)) =>
			// first transformer emitted some values that need to be forwarded to the second, before continuing
			second.stepMany(xEmit) map {
				case (outs, Left(leftovers)) =>
					// second transformer ended as a result of receiving values from the first
					outs -> None

				case (outs, Right(continueSecond)) =>
					// both transformers are still running, return the next state of the pipe
					val nextTransform = if(continueFirst.eq(first) && continueSecond.eq(second)) this else new TransformerThrough(continueFirst, continueSecond)
					outs -> Some(nextTransform)
			}
	}
	def finish: F[Emit[Out]] = first.finish flatMap { xEmit =>
		// forward the results of finishing the first parser along to the second parser
		F.flatMap(second.stepMany(xEmit)) {
			case (outs, Left(leftoverX)) =>
				// second parser finished as a result of the `xEmit` items, just return whatever it spit out as a result
				F.pure(outs)

			case (outs, Right(continueSecond)) =>
				// second parser is still active and needs to be finished; prepend the `outs` from the previous step to the final outputs
				F.map(continueSecond.finish) { outs ++ _ }
		}
	}
}