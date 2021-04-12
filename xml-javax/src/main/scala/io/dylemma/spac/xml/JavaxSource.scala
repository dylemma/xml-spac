package io.dylemma.spac.xml

import cats.effect.{Resource, Sync, SyncIO}
import fs2.Stream
import io.dylemma.spac.ChunkSize
import javax.xml.stream.XMLInputFactory

object JavaxSource {

	lazy val defaultFactory: XMLInputFactory = {
		val factory = XMLInputFactory.newInstance
		factory.setProperty(XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES, false)
		factory.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false)
		factory
	}

	def apply[F[_]] = new JavaxSourcePartiallyApplied[F]
	def syncIO = new JavaxSourcePartiallyApplied[SyncIO]

	class JavaxSourcePartiallyApplied[F[_]] {
		def iteratorResource[S](source: S)(implicit F: Sync[F], S: IntoXmlStreamReader[F, S], factory: XMLInputFactory = defaultFactory): Resource[F, Iterator[XmlEvent]] = {
			S(factory, source).flatMap { streamReader =>
				Resource.fromAutoCloseable(F.pure(new WrappedStreamReader(streamReader)))
			}
		}
		def apply[S](source: S)(implicit F: Sync[F], S: IntoXmlStreamReader[F, S], factory: XMLInputFactory = defaultFactory, chunkSize: ChunkSize = ChunkSize.default): Stream[F, XmlEvent] = {
			Stream
				.resource { iteratorResource(source) }
				.flatMap { Stream.fromBlockingIterator[F](_, chunkSize.i) }
		}
		def unmanaged[S](source: S)(implicit F: Sync[F], S: IntoXmlStreamReader[F, Resource[F, S]], factory: XMLInputFactory = defaultFactory, chunkSize: ChunkSize = ChunkSize.default): Stream[F, XmlEvent] = {
			apply(Resource.pure[F, S](source))
		}

	}

}
