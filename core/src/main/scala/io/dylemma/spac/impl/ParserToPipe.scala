package io.dylemma.spac
package impl

import fs2.{Pipe, Pull, Stream}

object ParserToPipe {

	def apply[F[_], In, Out](parser: Parser[In, Out], callerFrame: SpacTraceElement): Pipe[F, In, Out] = {
		stream => pullParsed(stream, parser.newHandler.asTopLevelHandler(callerFrame)).stream
	}

	def pullParsed[F[_], A, B](stream: Stream[F, A], handler: Parser.Handler[A, B]): Pull[F, B, Unit] = {
		stream.pull.uncons.flatMap {
			case Some((chunk, nextStream)) =>
				// feed the `head` to the parser...
				handler.stepMany(chunk) match {
					case Right(nextHandler) => pullParsed(nextStream, nextHandler)
					case Left((result, leftovers)) => Pull.output1(result)
				}
			case None =>
				// EOF; finish the parser, output its result, and stop pulling
				val result = handler.finish()
				Pull.output1(result)
		}
	}
}
