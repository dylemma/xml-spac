package io.dylemma.spac.json

import cats.effect.{Sync, SyncIO}
import com.fasterxml.jackson.core.JsonFactory
import io.dylemma.spac.{ChunkSize, Parsable}
import io.dylemma.spac.impl.ParsableByIterator

/** Provides implicits that allow various data sources to be passed to a JsonParser's `parse` method,
  * using the Jackson library's classes as the underlying JSON event generator.
  *
  * By default, a `Parsable` instance is made available for the following types:
  *
  *  - `String``
  *  - `java.io.File``
  *  - `Resource[F, java.io.InputStream]` (where `F` belongs to `Sync`)
  *  - `Resource[F, java.io.Reader]` (where `F` belongs to `Sync`)
  *
  *  A rough outline of the implicit derivation is:
  *  {{{
  *     Source => Resource[F, JacksonJsonParser] => Stream[F, JsonEvent] => Parsable[Source]
  *  }}}
  *
  * @group support */
object JacksonSupport {
	/** Provides a `Parsable` for applicable source types in any effect `F` that belongs to the `cats.effect.Sync` typeclass.
	  * This allows those source types to be passed to a JsonParser's `parseF` method, running the parser handler in the F context.
	  */
	implicit def jacksonParsableAsParsableF[F[_], S](
		implicit F: Sync[F],
		S: IntoJacksonJsonParser[F, S],
		factory: JsonFactory = JacksonSource.defaultFactory,
		chunkSize: ChunkSize = ChunkSize.default,
	): Parsable[F, S, JsonEvent] = {
		Parsable.forFs2Stream[F, JsonEvent].contramapSource(JacksonSource[F](_))
	}

	/** Provides a `Parsable` for applicable source types in the `cats.Id` context (using `cats.effect.SyncIO` under the hood).
	  * This allows those source types to be passed to a JsonParser's `parse` method, running the parser handler immediately.
	  */
	implicit def jacksonParsableAsParsable[S](
		implicit S: IntoJacksonJsonParser[SyncIO, S],
		factory: JsonFactory = JacksonSource.defaultFactory,
	): Parsable[cats.Id, S, JsonEvent] = new ParsableByIterator[S, JsonEvent] {
		protected def lendIterator[Out](source: S, f: Iterator[JsonEvent] => Out) = {
			JacksonSource.syncIO.iteratorResource(source).use(itr => SyncIO { f(itr) }).unsafeRunSync()
		}
	}
}
