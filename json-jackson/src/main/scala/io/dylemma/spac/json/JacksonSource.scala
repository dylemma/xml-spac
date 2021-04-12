package io.dylemma.spac
package json

import cats.effect.{Resource, Sync, SyncIO}
import com.fasterxml.jackson.core.JsonFactory
import fs2.Stream
import io.dylemma.spac.json.impl.JsonPopEventInjector

object JacksonSource {

	lazy val defaultFactory: JsonFactory = new JsonFactory

	def apply[F[_]] = new JacksonSourcePartiallyApplied[F]
	def syncIO = new JacksonSourcePartiallyApplied[SyncIO]

	class JacksonSourcePartiallyApplied[F[_]] {

		def iteratorResource[S](source: S)(implicit F: Sync[F], S: IntoJacksonJsonParser[F, S], factory: JsonFactory = defaultFactory): Resource[F, Iterator[JsonEvent]] = {
			for {
				jsonParser <- S(factory, source)
				wrappedJP <- Resource.fromAutoCloseable(F.delay { new WrappedJacksonParser(jsonParser) })
			} yield {
				JsonPopEventInjector.transform(wrappedJP)
			}
		}

		def apply[S](source: S)(implicit F: Sync[F], S: IntoJacksonJsonParser[F, S], factory: JsonFactory = defaultFactory, chunkSize: ChunkSize = ChunkSize.default): Stream[F, JsonEvent] = {
			Stream
				.resource { iteratorResource(source) }
				.flatMap { Stream.fromBlockingIterator[F](_, chunkSize.i) }
		}

		def unmanaged[S](source: S)(implicit F: Sync[F], S: IntoJacksonJsonParser[F, Resource[F, S]], factory: JsonFactory = defaultFactory, chunkSize: ChunkSize = ChunkSize.default): Stream[F, JsonEvent] = {
			apply(Resource.pure[F, S](source))
		}
	}

}
