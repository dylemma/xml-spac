package io.dylemma.spac.interop

import _root_.fs2.{Chunk, Compiler, Pipe, Stream}
import cats.MonadError
import cats.effect.{MonadCancel, Resource, Sync}
import io.dylemma.spac._
import io.dylemma.spac.interop.fs2.impl._

/** Provides implicits to allow for interop between the core SPaC classes and fs2 / cats-effect.
  *
  * - `Parser` gets `toPipe` and `parseF`
  * - `Transformer` gets `toPipe`
  * - `Source` gets `toResource` and `toStream`
  */
package object fs2 {

	implicit def unconsableForFs2Chunk: Unconsable[Chunk] = new Unconsable[Chunk] {
		def uncons[A](chunk: Chunk[A]) = {
			if (chunk.isEmpty) None
			else {
				val (headChunk, tail) = chunk.splitAt(1)
				headChunk.head.map(_ -> tail)
			}
		}
	}

	/** Since `Parser` is by design a stream consumer, we can provide the `parseF` helper which
	  * consumes a `fs2.Stream` in an effectful way. We can also provide the `toPipe` method, which transforms
	  * an input `fs2.Stream` to a new stream which emits exactly one value or raises an error.
	  *
	  * @param parser
	  * @tparam In
	  * @tparam Out
	  */
	implicit class ParserFs2Ops[In, Out](private val parser: Parser[In, Out]) extends AnyVal {
		/** Convert this parser to a FS2 "Pipe".
		  * The resulting pipe will forward inputs from the upstream into this parser,
		  * emitting a single value to the downstream when this parser finishes.
		  * Since a `Parser` may abort early (e.g. with `Parser.first`),
		  * the pipe may not pull the entire input stream.
		  *
		  * @param pos
		  * @tparam F
		  * @return
		  * @group consumers
		  */
		def toPipe[F[_]](implicit pos: CallerPos): Pipe[F, In, Out] = {
			ParserToPipe(parser, SpacTraceElement.InParse("parser", "toPipe", pos))
		}

		/** Convenience for `stream.through(parser.toPipe).compile.lastOrError`.
		  *
		  * Uses this parser to pull from the `stream` until the parser emits a result or the stream is depleted.
		  *
		  * @param stream The stream of events (of type `In`) to consume
		  * @param compiler The fs2 stream compiler
		  * @param G Evidence that the `G` effect type can raise Throwables as errors
		  * @param pos call-point information used to generate the top SpacTraceElement for error handling
		  * @tparam F The stream effect type, e.g. `cats.effect.IO`
		  * @tparam G The stream-compiler output type. Usually the same as `F`
		  * @group consumers
		  * @return
		  */
		def parseF[F[_], G[_]](stream: Stream[F, In])(implicit compiler: Compiler[F, G], G: MonadError[G, Throwable], pos: CallerPos): G[Out] = {
			stream.through(ParserToPipe(parser, SpacTraceElement.InParse("parser", "parseF", pos))).compile.lastOrError
		}
	}

	/** Since a `Transformer` is by design a stream transformation, we can naturally provide a conversion from `Transformer`
	  * to `fs2.Pipe`
	  *
	  * @param transformer
	  * @tparam In
	  * @tparam Out
	  */
	implicit class TransformerFs2Ops[In, Out](private val transformer: Transformer[In, Out]) extends AnyVal {
		/** Convert this transformer to a `Pipe` which will apply this transformer's logic to an fs2 `Stream`.
		  *
		  * @param pos Captures the caller filename and line number, used to fill in the 'spac trace' if the parser throws an exception
		  * @tparam F Effect type for the Pipe/Stream
		  * @return An `fs2.Pipe[F, In, Out]` that will apply this transformer's logic
		  * @group transform
		  */
		def toPipe[F[_]](implicit pos: CallerPos): Pipe[F, In, Out] = TransformerToPipe(transformer, SpacTraceElement.InParse("transformer", "toPipe", pos))
	}

	/** Since `Source` is a synchronous-only encoding of the `Resource` pattern, it can
	  * be converted to a `cats.effect.Resource` by suspending its `open` and `close`
	  * operations in a Sync effect type `F`, yielding an `Iterator[A]` as its value.
	  *
	  * This can be taken a step further by lifting that `Resource` to a `fs2.Stream`
	  * and wrapping the provided Iterator as a stream, to treat the whole `Source[A]`
	  * as a `fs2.Stream[F, A]`
	  *
	  * @param source
	  * @tparam A
	  */
	implicit class SourceFs2Ops[A](private val source: Source[A]) extends AnyVal {

		/** Upgrades this `Source` to a cats-effect `Resource` of the given effect type `F`.
		  * The open and close operations of the underlying source are assumed to be blocking,
		  * so they are wrapped with `Sync[F].blocking { ... }`.
		  *
		  * @tparam F The effect type
		  * @return A new Resource which delegates to this Source's `open` method
		  */
		def toResource[F[_]: Sync]: Resource[F, Iterator[A]] = {
			val F = Sync[F]
			Resource(F.blocking {
				val (itr, close) = source.open()
				val closeF = F.blocking { close() }
				itr -> closeF
			})
		}

		/** Converts this `Source` to an fs2 `Stream` in the given effect type `F`.
		  *
		  * Uses [[toResource]] to encapsulate the underlying open/close operation,
		  * and uses `Stream.fromBlockingIterator` to wrap the underlying Iterator provided by the Source.
		  * The underlying Iterator is assumed to use blocking operations internally since typically
		  * the Iterator would be backed by something like a `java.io.InputStream`.
		  *
		  * @param chunkSize The number of times the underlying Iterator's `next` should be called, per blocking step
		  * @param F Sync[F] typeclass instance
		  * @param FM MonadCancel[F, _] typeclass instance
		  * @tparam F The effect type
		  * @return A Stream over the data provided by the underlying Source
		  */
		def toStream[F[_]](chunkSize: Int = 32)(implicit F: Sync[F], FM: MonadCancel[F, _]): Stream[F, A] = {
			Stream.resource(toResource[F]).flatMap(Stream.fromBlockingIterator[F](_, chunkSize))
		}
	}
}
