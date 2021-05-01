package io.dylemma.spac
package json

import cats.effect.{Resource, Sync, SyncIO}
import com.fasterxml.jackson.core.JsonFactory
import fs2.Stream
import io.dylemma.spac.json.impl.JsonStackFixer

/** Provides helpers for creating `fs2.Stream` and `Iterator` instances of `JsonEvent` from various underlying event sources,
  * using the Jackson library as the underlying event parser.
  *
  * The helpers in this object operate in terms of the `IntoJacksonJsonParser` typeclass, which defines logic for
  * using a `source` value to construct a Jackson JsonParser in a Resource. From there, the helpers take care of
  * converting the jackson parser/event model to the spac JsonEvent model.
  *
  * For example:
  * {{{
  *    val file = new File("./stuff.json")
  *    val jsonStream: Stream[IO, JsonEvent] = JacksonSource[IO](file)
  * }}}
  *
  * @group support */
object JacksonSource {

	/** Default `JsonFactory` used by the helpers in this object when interfacing with the `IntoJacksonJsonParser` typeclass
	  * to create the underlying Jackson JsonParsers.
	  *
	  * This is a `new JsonFactory` with no additional modifications.
	  */
	lazy val defaultFactory: JsonFactory = new JsonFactory

	/** Partial apply method for creating a `Stream[F, JsonEvent]` or a `Resource[F, Iterator[JsonEvent]]`
	  *
	  * Example usage:
	  * {{{
	  *    val file = new java.io.File("./stuff.json")
	  *    JacksonSource[IO].iteratorResource(file)
	  *    JacksonSource[IO](file)
	  *    JacksonSource[IO].unmanaged(new ByteArrayInputStream(???))
	  * }}}
	  */
	def apply[F[_]] = new JacksonSourcePartiallyApplied[F]

	/** Alias for `apply[SyncIO]`
	  *
	  * Example usage:
	  * {{{
	  *    val file = new java.io.File("./stuff.json")
	  *    JacksonSource.syncIO.iteratorResource(file)
	  *    JacksonSource.syncIO(file)
	  *    JacksonSource.syncIO.unmanaged(new ByteArrayInputStream(???))
	  * }}}
	  */
	def syncIO = new JacksonSourcePartiallyApplied[SyncIO]

	/** Intermediate object returned by `apply` and `syncIO`, with methods for actually creating event streams. */
	class JacksonSourcePartiallyApplied[F[_]] {

		/** Treats the given `source` as an `Resource` which allocates an Iterator of JsonEvents.
		  * The resource will open the underlying file/stream/whatever, wrapping it as an Iterator,
		  * and will close that underlying resource when the resource is closed.
		  *
		  * Example usage:
		  * {{{
		  *    JacksonSource
		  *      .syncIO
		  *      .iteratorResource(new File("./stuff.json"))
		  *      .use { _.foreach(println) }
		  *      .unsafeRunSync()
		  * }}}
		  *
		  * @param source  Some source of JSON data, e.g. a File or String, or a Resource that allocates an InputStream or Reader
		  * @param F       Evidence that the `F[_]` type is "Sync"
		  * @param S       Evidence that the `source` can be "opened" as a jackson json parser
		  * @param factory The JsonFactory used to construct the underlying jackson json parser. Defaults to `defaultFactory` if no other factory is available in implicit scope
		  * @tparam S The source type
		  * @return A resource which can allocate an iterator of JsonEvents originating from the `source`
		  */
		def iteratorResource[S](source: S)(implicit F: Sync[F], S: IntoJacksonJsonParser[F, S], factory: JsonFactory = defaultFactory): Resource[F, Iterator[JsonEvent]] = {
			for {
				jsonParser <- S(factory, source)
				wrappedJP <- Resource.fromAutoCloseable(F.delay {new WrappedJacksonParser(jsonParser)})
			} yield {
				JsonStackFixer.transform(wrappedJP)
			}
		}

		/** Treats the given `source` as an `fs2.Stream` of JsonEvent.
		  *
		  * The opening and closing of underlying resources like File handles or streams will be handled internally by the returned stream.
		  *
		  * Example usage:
		  * {{{
		  *    JacksonSource[IO](new File("./stuff.json"))
		  *      .compile
		  *      .toList
		  *      .unsafeRunSync()
		  * }}}
		  *
		  * @param source    Some source of JSON data, e.g. a File or String, or a Resource that allocates an InputStream or Reader
		  * @param F         Evidence that the `F[_]` type is "Sync"
		  * @param S         Evidence that the `source` can be "opened" as a jackson json parser
		  * @param factory   The JsonFactory used to construct the underlying jackson json parser. Defaults to `defaultFactory` if no other factory is available in implicit scope
		  * @param chunkSize The number of elements to pull from the underlying jackson json parser at once.
		  *                  Under the hood, the returned stream uses `Stream.fromBlockingIterator`; this parameter is passed as the `chunkSize` to that call.
		  *                  Defaults to 25. To override this, define an implicit `ChunkSize` in implicit scope before calling this method.
		  * @tparam S The source type
		  * @return An `fs2.Stream[F, JsonEvent]` which when run will open the underlying source and parse JsonEvents from it
		  */
		def apply[S](source: S)(implicit F: Sync[F], S: IntoJacksonJsonParser[F, S], factory: JsonFactory = defaultFactory, chunkSize: ChunkSize = ChunkSize.default): Stream[F, JsonEvent] = {
			Stream
				.resource {iteratorResource(source)}
				.flatMap {Stream.fromBlockingIterator[F](_, chunkSize.i)}
		}

		/** Treats the given `source` as an `fs2.Stream` of JsonEvent, but does not attempt to open or close the source.
		  *
		  * This is essentially an alias for `apply` in cases where the source is an `InputStream` or `Reader` that should not be closed.
		  * Normally, InputStream and Reader are not directly supported by the `IntoJacksonJsonParser` typeclass.
		  * Instead, they must be wrapped in a `Resource` to dictate the open/close semantics.
		  * This method automatically wraps the `source` with `Resource.pure`, effectively disabling the `close()` method
		  * on the source for streaming purposes.
		  *
		  * Note that the stream returned by this method should not be run more than once, since running the stream will
		  * cause the `source` stream/reader to advance.
		  *
		  * Example usage:
		  * {{{
		  *    val rawJsonBytes: Array[Byte] = ???
		  *    val eventsList: List[JsonEvent] = JacksonSource[IO]
		  *      .unmanaged(new ByteArrayInputStream(rawJsonBytes))
		  *      .compile
		  *      .toList
		  *      .unsafeRunSync()
		  * }}}
		  *
		  * @param source    An InputStream or Reader from which to pull raw JSON data. Neither this method nor the stream
		  *                  it returns will attempt to `close` the source.
		  * @param F         Evidence that the `F[_]` type is "Sync"
		  * @param S         Evidence that the `source` could be "opened" as an jackson json parser if it had first been wrapped as a `Resource`
		  * @param factory   The JsonFactory used to construct the underlying jackson json parser. Defaults to `defaultFactory` if no other factory is available in implicit scope
		  * @param chunkSize The number of elements to pull from the underlying jackson json parser at once.
		  *                  Under the hood, the returned stream uses `Stream.fromBlockingIterator`; this parameter is passed as the `chunkSize` to that call.
		  *                  Defaults to 25. To override this, define an implicit `ChunkSize` in implicit scope before calling this method.
		  * @tparam S The source type; for this method it will either be `java.io.InputStream` or `java.io.Reader`.
		  * @return An `fs2.Stream[F, JsonEvent]` which when run will consume the `source` to pull JsonEvents from it
		  */
		def unmanaged[S](source: S)(implicit F: Sync[F], S: IntoJacksonJsonParser[F, Resource[F, S]], factory: JsonFactory = defaultFactory, chunkSize: ChunkSize = ChunkSize.default): Stream[F, JsonEvent] = {
			apply(Resource.pure[F, S](source))
		}
	}

}
