package io.dylemma.spac

import cats.data.Chain
import cats.effect.{Sync, SyncIO}
import cats.~>
import fs2.Stream
import io.dylemma.spac.impl.{ParsableByIterator, ParserToPipe}

/** Typeclass used to provide functionality to `Parser#parse` and `Parser#parseF`.
  *
  * Implementations of `Parsable` are responsible for calling a parser's `newHandler`
  * method, then feeding events to that handler until either the handler finishes on its own,
  * or there are no more events.
  *
  * Extra instances of `Parsable` are made available via separate spac "support" libraries,
  * e.g. via `JavaxSupport` to allow Files etc to be parsed as XmlEvents.
  *
  * @tparam F The "effect" type, or `cats.Id` to indicate the `parse` operation is "blocking".
  *           Note that the use of `cats.Id` in this context doesn't mean it is not allowed
  *           to evaluate side-effects; it's just a way of escaping from having everything
  *           wrapped in `F`.
  * @tparam S The "source" type. Typically an `Iterable[In]`, or a `java.io.File`
  * @tparam In The "event" type, i.e. the parser's input type
  */
trait Parsable[F[_], -S, +In] { self =>
	def parse[Out](source: S, callerFrame: SpacTraceElement, parser: Parser[In, Out]): F[Out]

	def mapK[G[_]](f: F ~> G): Parsable[G, S, In] = new Parsable[G, S, In] {
		def parse[Out](source: S, callerFrame: SpacTraceElement, parser: Parser[In, Out]): G[Out] = {
			f(self.parse(source, callerFrame, parser))
		}
	}

	def contramapSource[S2](f: S2 => S): Parsable[F, S2, In] = new Parsable[F, S2, In] {
		def parse[Out](source: S2, callerFrame: SpacTraceElement, parser: Parser[In, Out]) = {
			self.parse(f(source), callerFrame, parser)
		}
	}
}
object Parsable {

	implicit def forIterable[In]: Parsable[cats.Id, Iterable[In], In] = new ParsableByIterator[Iterable[In], In] {
		protected def lendIterator[Out](source: Iterable[In], f: Iterator[In] => Out) = f(source.iterator)
	}
	// For some reason Chain isn't `Iterable`, so we need a separate implicit for Chain.
	implicit def forChain[In]: Parsable[cats.Id, Chain[In], In] = new ParsableByIterator[Chain[In], In] {
		protected def lendIterator[Out](source: Chain[In], f: Iterator[In] => Out) = f(source.iterator)
	}

	implicit def forFs2StreamSyncIO[In]: Parsable[cats.Id, Stream[SyncIO, In], In] = new Parsable[cats.Id, Stream[SyncIO, In], In] {
		def parse[Out](source: Stream[SyncIO, In], callerFrame: SpacTraceElement, parser: Parser[In, Out]) = {
			source.through(ParserToPipe(parser, callerFrame)).compile.lastOrError.unsafeRunSync()
		}
	}

	implicit def forFs2Stream[F[_] : Sync, In]: Parsable[F, Stream[F, In], In] = new Parsable[F, Stream[F, In], In] {
		def parse[Out](source: Stream[F, In], callerFrame: SpacTraceElement, parser: Parser[In, Out]) = {
			source.through(ParserToPipe(parser, callerFrame)).compile.lastOrError
		}
	}
}

