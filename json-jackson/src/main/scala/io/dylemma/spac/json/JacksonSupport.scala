package io.dylemma.spac.json

import cats.effect.{Sync, SyncIO}
import com.fasterxml.jackson.core.JsonFactory
import io.dylemma.spac.{ChunkSize, Parsable}
import io.dylemma.spac.impl.ParsableByIterator

object JacksonSupport {
	implicit def jacksonParsableAsParsableF[F[_], S](
		implicit F: Sync[F],
		S: IntoJacksonJsonParser[F, S],
		factory: JsonFactory = JacksonSource.defaultFactory,
		chunkSize: ChunkSize = ChunkSize.default,
	): Parsable[F, S, JsonEvent] = {
		Parsable.forFs2Stream[F, JsonEvent].contramapSource(JacksonSource[F](_))
	}

	implicit def jacksonParsableAsParsable[S](
		implicit S: IntoJacksonJsonParser[SyncIO, S],
		factory: JsonFactory = JacksonSource.defaultFactory,
	): Parsable[cats.Id, S, JsonEvent] = new ParsableByIterator[S, JsonEvent] {
		protected def lendIterator[Out](source: S, f: Iterator[JsonEvent] => Out) = {
			JacksonSource.syncIO.iteratorResource(source).use(itr => SyncIO { f(itr) }).unsafeRunSync()
		}
	}
}
