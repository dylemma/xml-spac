package io.dylemma.spac

import cats.Monad
import fs2.{Chunk, Pipe, Stream, Pull => Fs2Pull}
import io.dylemma.spac.types.Unconsable

object Fs2Support {

	implicit class TransformerOps[A, B](transformer: Transformer[A, B]) {
		def toPipe[F[_]: Monad]: Pipe[F, A, B] = {
			stream => pullTransformed(stream, transformer).stream
		}
	}

	implicit val fs2ChunkUnconsable: Unconsable[Chunk] = new Unconsable[Chunk] {
		def uncons[A](chunk: Chunk[A]) = {
			if (chunk.isEmpty) None
			else {
				val (headChunk, tail) = chunk.splitAt(1)
				headChunk.head.map(_ -> tail)
			}
		}
	}

	/*
	Note: all of the `flatMap` calls in this file have explicit type hints on them in order to work
	around a type inference issue that affects scala 2.12. Since F is covariant, it seems to be confusing
	the compiler when fulfilling the `F2[x] >: F[x]` part of the flatMap signature.
	 */
	private def pullTransformed[F[_]: Monad, A, B](stream: Stream[F, A], transformer: Transformer[A, B]): Fs2Pull[F, B, Unit] = {
		Fs2Pull.suspend { Fs2Pull.pure[F, Transformer.Handler[A, B]](transformer.newHandler) } flatMap { handler =>
			pullTransformed(stream, handler)
		}
	}
	private def pullTransformed[F[_]: Monad, A, B](stream: Stream[F, A], handler: Transformer.Handler[A, B]): Fs2Pull[F, B, Unit] = {
		stream.pull.uncons.flatMap {
			case Some((chunk, nextStream)) =>
				handler.stepMany(chunk) match {
					case (toEmit, Right(nextTransformer)) => Fs2Pull.output(Chunk.chain(toEmit)) >> pullTransformed(nextStream, nextTransformer)
					case (toEmit, Left(leftovers)) => Fs2Pull.output(Chunk.chain(toEmit))
				}
			case None =>
				// EOF - finish the transformer and emit what it produces
				val finalEmit = handler.finish()
				Fs2Pull.output(Chunk.chain(finalEmit))
		}
	}

	implicit class PullableOps[S](source: S) {
		def toFs2Stream[F[+_], A](implicit toPull: ToPullable[F, S, A]): Stream[F, A] = {
			Stream.resource(toPull(source)).flatMap[F[*], A] { start =>
				Stream.unfoldEval[F, Pullable[F, A], A](start)(_.uncons)
			}
		}
	}

	implicit class ParserOps[F[+_], A, B](parser: Parser[A, B]) {
		def toPipe: Pipe[F, A, B] = {
			stream => pullParsed(stream, parser).stream
		}
	}

	private def pullParsed[F[_], A, B](stream: Stream[F, A], parser: Parser[A, B]): Fs2Pull[F, B, Unit] = {
		Fs2Pull.suspend { Fs2Pull.pure(parser.newHandler) }.flatMap { handler =>
			pullParsed(stream, handler)
		}
	}
	private def pullParsed[F[_], A, B](stream: Stream[F, A], handler: Parser.Handler[A, B]): Fs2Pull[F, B, Unit] = {
		stream.pull.uncons.flatMap {
			case Some((chunk, nextStream)) =>
				// feed the `head` to the parser...
				handler.stepMany(chunk) match {
					case Right(nextHandler) => pullParsed(nextStream, nextHandler)
					case Left((result, leftovers)) => Fs2Pull.output1(result)
				}
			case None =>
				// EOF; finish the parser, output its result, and stop pulling
				val result = handler.finish()
				Fs2Pull.output1(result)
		}
	}
}
