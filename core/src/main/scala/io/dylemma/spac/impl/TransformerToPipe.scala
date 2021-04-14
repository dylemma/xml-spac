package io.dylemma.spac
package impl

import cats.Monad
import fs2.{Chunk, Pipe, Pull, Stream}

object TransformerToPipe {
	def apply[F[_]: Monad, A, B](t: Transformer[A, B], caller: SpacTraceElement): Pipe[F, A, B] = {
		stream => pullTransformed(stream, t.newHandler.asTopLevelHandler(caller)).stream
	}

	def pullTransformed[F[_]: Monad, A, B](stream: Stream[F, A], transformer: Transformer[A, B]): Pull[F, B, Unit] = {
		Pull.suspend { Pull.pure[F, Transformer.Handler[A, B]](transformer.newHandler) } flatMap { handler =>
			pullTransformed(stream, handler)
		}
	}

	def pullTransformed[F[_]: Monad, A, B](stream: Stream[F, A], handler: Transformer.Handler[A, B]): Pull[F, B, Unit] = {
		stream.pull.uncons.flatMap {
			case Some((chunk, nextStream)) =>
				handler.stepMany(chunk) match {
					case (toEmit, Right(nextTransformer)) => Pull.output(Chunk.chain(toEmit)) >> pullTransformed(nextStream, nextTransformer)
					case (toEmit, Left(leftovers)) => Pull.output(Chunk.chain(toEmit))
				}
			case None =>
				// EOF - finish the transformer and emit what it produces
				val finalEmit = handler.finish()
				Pull.output(Chunk.chain(finalEmit))
		}
	}
}
