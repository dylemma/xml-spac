package io.dylemma.spac.xml

import cats.effect.{Resource, Sync, SyncIO}
import fs2.Stream
import io.dylemma.spac.ChunkSize
import javax.xml.stream.XMLInputFactory

/** Provides convenience constructors for Iterators and Streams of `XmlEvent` for source types belonging to the `IntoXmlStreamReader` typeclass.
  *
  * @group support
  */
object JavaxSource {

	/** Default `XMLInputFactory` used when creating an underlying `XMLStreamReader`
	  * with the methods in this object.
	  *
	  * This factory disables the `IS_REPLACING_ENTITY_REFERENCES` and `IS_SUPPORTING_EXTERNAL_ENTITIES`
	  * features, in efforts to mitigate xml injection attacks.
	  *
	  * When using the methods in this object, if you want to override this default factory,
	  * define an implicit `XMLInputFactory` somewhere and make it available in the scope
	  * where you call the method, e.g.
	  * {{{
	  *    implicit val mySpecificXmlFactory: XMLInputFactory = ???
	  *    val xmlEvents = JavaxSource[IO](new File("./stuff.xml"))
	  * }}}
	  */
	lazy val defaultFactory: XMLInputFactory = {
		val factory = XMLInputFactory.newInstance
		factory.setProperty(XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES, false)
		factory.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false)
		factory
	}

	/** Partial apply for JavaxSource, where the `F` you pass will be the effect type.
	  *
	  * Example usage:
	  * {{{
	  *   val file = new java.io.File("./stuff.xml")
	  *   JavaxSource[IO].iteratorResource(file)
	  *   JavaxSource[IO](file)
	  *   JavaxSource[IO].unmanaged(new ByteArrayInputStream(???))
	  * }}}
	  *
	  * @tparam F The effect type used by the stream/resource when finishing this "partial apply"
	  * @return An intermediate object with methods that let you construct a stream or resource
	  */
	def apply[F[_]] = new JavaxSourcePartiallyApplied[F]

	/** Alias for `apply[SyncIO]`.
	  *
	  * Example usage:
	  * {{{
	  *    val file = new java.io.File("./stuff.xml")
	  *    JavaxSource.syncIO.iteratorResource(file)
	  *    JavaxSource.syncIO(file)
	  *    JavaxSource.syncIO.unmanaged(new ByteArrayInputStream(???))
	  * }}}
	  */
	def syncIO = new JavaxSourcePartiallyApplied[SyncIO]

	/** Intermediate object returned by `apply` and `syncIO`, with methods for actually creating event streams. */
	class JavaxSourcePartiallyApplied[F[_]] {
		/** Treats the given `source` as an `Resource` which allocates an Iterator of XmlEvents.
		  * The resource will open the underlying file/stream/whatever, wrapping it as an Iterator,
		  * and will close that underlying resource when the resource is closed.
		  *
		  * Example usage:
		  * {{{
		  *    JavaxSource
		  *      .syncIO
		  *      .iteratorResource(new File("./stuff.xml"))
		  *      .use { _.foreach(println) }
		  *      .unsafeRunSync()
		  * }}}
		  *
		  * @param source Some source of XML data, e.g. a File or String, or a Resource that allocates an InputStream or Reader
		  * @param F Evidence that the `F[_]` type is "Sync"
		  * @param S Evidence that the `source` can be "opened" as an XMLStreamReader
		  * @param factory The XMLInputFactory used to construct the underlying XML event reader. Defaults to `defaultFactory` if no other factory is available in implicit scope
		  * @tparam S The source type
		  * @return A resource which can allocate an iterator of XMLEvents originating from the `source`
		  */
		def iteratorResource[S](source: S)(implicit F: Sync[F], S: IntoXmlStreamReader[F, S], factory: XMLInputFactory = defaultFactory): Resource[F, Iterator[XmlEvent]] = {
			S(factory, source).flatMap { streamReader =>
				Resource.fromAutoCloseable(F.pure(new WrappedStreamReader(streamReader)))
			}
		}

		/** Treats the given `source` as an `fs2.Stream` of XmlEvent.
		  *
		  * The opening and closing of underlying resources like File handles or streams will be handled internally by the returned stream.
		  *
		  * Example usage:
		  * {{{
		  *    JavaxSource[IO](new File("./stuff.xml"))
		  *      .compile
		  *      .toList
		  *      .unsafeRunSync()
		  * }}}
		  *
		  * @param source Some source of XML data, e.g. a File or String, or a Resource that allocates an InputStream or Reader
		  * @param F Evidence that the `F[_]` type is "Sync"
		  * @param S Evidence that the `source` can be "opened" as an XMLStreamReader
		  * @param factory The XMLInputFactory used to construct the underlying XML event reader. Defaults to `defaultFactory` if no other factory is available in implicit scope
		  * @param chunkSize The number of elements to pull from the underlying XMLStreamReader at once.
		  *                  Under the hood, the returned stream uses `Stream.fromBlockingIterator`; this parameter is passed as the `chunkSize` to that call.
		  *                  Defaults to 25. To override this, define an implicit `ChunkSize` in implicit scope before calling this method.
		  * @tparam S The source type
		  * @return An `fs2.Stream[F, XmlEvent]` which when run will open the underlying source and parse XmlEvents from it
		  */
		def apply[S](source: S)(implicit F: Sync[F], S: IntoXmlStreamReader[F, S], factory: XMLInputFactory = defaultFactory, chunkSize: ChunkSize = ChunkSize.default): Stream[F, XmlEvent] = {
			Stream
				.resource { iteratorResource(source) }
				.flatMap { Stream.fromBlockingIterator[F](_, chunkSize.i) }
		}

		/** Treats the given `source` as an `fs2.Stream` of XmlEvent, but does not attempt to open or close the source.
		  *
		  * This is essentially an alias for `apply` in cases where the source is an `InputStream` or `Reader` that should not be closed.
		  * Normally, InputStream and Reader are not directly supported by the `IntoXmlStreamReader` typeclass.
		  * Instead, they must be wrapped in a `Resource` to dictate the open/close semantics.
		  * This method automatically wraps the `source` with `Resource.pure`, effectively disabling the `close()` method
		  * on the source for streaming purposes.
		  *
		  * Note that the stream returned by this method should not be run more than once, since running the stream will
		  * cause the `source` stream/reader to advance.
		  *
		  * Example usage:
		  * {{{
		  *    val rawXmlBytes: Array[Byte] = ???
		  *    val eventsList: List[XmlEvent] = JavaxSource[IO]
		  *      .unmanaged(new ByteArrayInputStream(rawXmlBytes))
		  *      .compile
		  *      .toList
		  *      .unsafeRunSync()
		  * }}}
		  *
		  * @param source An InputStream or Reader from which to pull raw XML data. Neither this method nor the stream
		  *               it returns will attempt to `close` the source.
		  * @param F Evidence that the `F[_]` type is "Sync"
		  * @param S Evidence that the `source` could be "opened" as an XMLStreamReader if it had first been wrapped as a `Resource`
		  * @param factory The XMLInputFactory used to construct the underlying XML event reader. Defaults to `defaultFactory` if no other factory is available in implicit scope
		  * @param chunkSize The number of elements to pull from the underlying XMLStreamReader at once.
		  *                  Under the hood, the returned stream uses `Stream.fromBlockingIterator`; this parameter is passed as the `chunkSize` to that call.
		  *                  Defaults to 25. To override this, define an implicit `ChunkSize` in implicit scope before calling this method.
		  * @tparam S The source type; for this method it will either be `java.io.InputStream` or `java.io.Reader`.
		  * @return An `fs2.Stream[F, XmlEvent]` which when run will consume the `source` to pull XmlEvents from it
		  */
		def unmanaged[S](source: S)(implicit F: Sync[F], S: IntoXmlStreamReader[F, Resource[F, S]], factory: XMLInputFactory = defaultFactory, chunkSize: ChunkSize = ChunkSize.default): Stream[F, XmlEvent] = {
			apply(Resource.pure[F, S](source))
		}

	}

}
