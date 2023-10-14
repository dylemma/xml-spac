package io.dylemma.spac.interop.fs2.impl

import _root_.fs2.{Chunk, Pipe, Pull, Stream}
import io.dylemma.spac.{SpacTraceElement, Transformer}

import scala.collection.immutable.VectorBuilder

object TransformerToPipe {
	private type Handlers[A, B] = (Transformer.BoundHandler[A], Transformer.BoundHandler.ToBuilder[B, Vector[B]])

	def apply[F[_], A, B](t: Transformer[A, B], caller: SpacTraceElement): Pipe[F, A, B] = init => Pull
		.suspend {
			val downstream = new Transformer.BoundHandler.ToBuilder(new VectorBuilder[B])
			val handler = Transformer.Handler.bindDownstream(t.newHandler.asTopLevelHandler(caller), downstream)
			Pull.pure[F, Handlers[A, B]](handler -> downstream)
		}
		.flatMap { case (handler, downstream) =>
			def pullTransformed(stream: Stream[F, A]): Pull[F, B, Unit] = stream.pull.uncons.flatMap {
				case Some((chunk, tail)) =>
					// send the chunk of inputs to the handler, which may cause the downstream to receive some outputs,
					// which we can then `take()` and turn into a chunk to emit.
					// Then as long as the handler signal wasn't `Stop`, recurse to pull the stream `tail`
					val signal = handler.pushMany(chunk.iterator)
					val toEmit = Chunk.from(downstream.take())
					if (toEmit.isEmpty && signal.isStop) Pull.done
					else if (toEmit.isEmpty) pullTransformed(tail)
					else Pull.output(toEmit) >> pullTransformed(tail)

				case None =>
					// EOF - finish the handler and emit whatever it might produce
					handler.finish()
					Pull.output(Chunk.from(downstream.take()))
			}

			pullTransformed(init)
		}
		.stream
}
