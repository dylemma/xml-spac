package io.dylemma.spac.impl

import cats.data.Chain
import cats.implicits._
import cats.{Applicative, Functor, Monad}
import io.dylemma.spac.{Emit, Parser, Transformer}

class TransformerIdentity[F[+_], In](implicit F: Applicative[F]) extends Transformer[F, In, In] {
	def step(in: In): F[(Emit[In], Option[Transformer[F, In, In]])] = F.pure(Chain.one(in) -> Some(this))
	def finish: F[Emit[In]] = F.pure(Chain.nil)
}

class TransformerMapEmit[F[+_], -In, +Out](f: In => Emit[Out])(implicit F: Applicative[F]) extends Transformer[F, In, Out] {
	def step(_in: In): F[(Emit[Out], Option[Transformer[F, In, Out]])] = _in.pure[F].map { in => f(in) -> Some(this) }
	def finish: F[Emit[Out]] = F.pure(Chain.nil)
}

class TransformerPipe[F[+_], -In, X, +Out](first: Transformer[F, In, X], second: Transformer[F, X, Out])(implicit F: Monad[F]) extends Transformer[F, In, Out] {
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
					val nextTransform = if(continueFirst.eq(first) && continueSecond.eq(second)) this else new TransformerPipe(continueFirst, continueSecond)
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

class TransformedParser[F[+_]: Monad, -In, X, Out](transformer: Transformer[F, In, X], parser: Parser[F, X, Out]) extends Parser[F, In, Out] {
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
				case Right(nextParser) => Right(new TransformedParser(nextTransformer, nextParser))
			}
	}
	def finish: F[Out] = transformer.finish.flatMap { xEmit =>
		parser.stepMany(xEmit) flatMap {
			case Left((out, leftovers)) => out.pure[F]
			case Right(finalParser) => finalParser.finish
		}
	}
}

class TransformerMapBatch[F[+_], -In, Out, +Out2](inner: Transformer[F, In, Out], f: Emit[Out] => Emit[Out2])(implicit F: Functor[F]) extends Transformer[F, In, Out2] {
	def step(in: In): F[(Emit[Out2], Option[Transformer[F, In, Out2]])] = F.map(inner.step(in)) {
		case (emit, nextInnerOpt) =>
			val nextEmit = f(emit)
			val nextTransformer = nextInnerOpt.map {
				case `inner` => this // "pure" transformers don't require extra allocations since there's no state change
				case nextInner => new TransformerMapBatch(nextInner, f)
			}
			nextEmit -> nextTransformer
	}
	def finish: F[Emit[Out2]] = F.map(inner.finish)(f)
}