package io.dylemma.spac.pure // TODO repackage

import cats.Monad
import fs2.{Chunk, Pipe, Stream, Pull => Fs2Pull}
import io.dylemma.spac.{Parser, Pullable, ToPullable, Transformer}

object Fs2Support {

	implicit class TransformerOps[F[+_]: Monad, A, B](transformer: Transformer[F, A, B]) {
		def toPipe: Pipe[F, A, B] = {
			stream => pullTransformed(stream, transformer).stream
		}
	}

	private def pullTransformed[F[+_]: Monad, A, B](stream: Stream[F, A], transformer: Transformer[F, A, B]): Fs2Pull[F, B, Unit] = {
		stream.pull.uncons1.flatMap {
			case Some((a, nextStream)) =>
				Fs2Pull.eval(transformer.step(a)).flatMap {
					case (toEmit, Some(nextTransformer)) => Fs2Pull.output(Chunk.chain(toEmit)) >> pullTransformed(nextStream, nextTransformer)
					case (toEmit, None) => Fs2Pull.output(Chunk.chain(toEmit))
				}
			case None =>
				// EOF - finish the transformer and emit what it produces
				Fs2Pull.eval(transformer.finish).flatMap { finalEmit =>
					Fs2Pull.output(Chunk.chain(finalEmit))
				}
		}
	}

	implicit class PullableOps[S](source: S) {
		def toFs2Stream[F[+_], A](implicit toPull: ToPullable[F, S, A]): Stream[F, A] = {
			Stream.resource(toPull(source)).flatMap { start =>
				Stream.unfoldEval[F, Pullable[F, A], A](start)(_.uncons)
			}
		}
	}

	implicit class ParserOps[F[+_], A, B](parser: Parser[F, A, B]) {
		def toPipe: Pipe[F, A, B] = {
			stream => pullParsed(stream, parser).stream
		}
	}

	private def pullParsed[F[+_], A, B](stream: Stream[F, A], parser: Parser[F, A, B]): Fs2Pull[F, B, Unit] = {
		stream.pull.uncons1.flatMap {
			case Some((head, nextStream)) =>
				// feed the `head` to the parser...
				Fs2Pull.eval(parser.step(head)).flatMap {
					case Left(result) => Fs2Pull.output1(result)
					case Right(nextParser) => pullParsed(nextStream, nextParser)
				}
			case None =>
				// EOF; finish the parser, output its result, and stop pulling
				Fs2Pull.eval(parser.finish).flatMap(Fs2Pull.output1)
		}
	}
}
