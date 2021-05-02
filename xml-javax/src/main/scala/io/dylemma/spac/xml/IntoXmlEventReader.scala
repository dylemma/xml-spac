package io.dylemma.spac
package xml

import cats.Applicative
import cats.effect.{Resource, Sync}

import java.io._
import javax.xml.stream.{XMLEventReader, XMLInputFactory}
import scala.io.Codec

/** Typeclass representing the conversion of some `source` into a javax `XMLEventReader`,
  * used by `JavaxSource` and `JavaxSupport`.
  *
  * The opening and closing of underlying resources like file/stream handles is represented with `cats.effect.Resource`,
  * where the result of "converting" a source is a `Resource[F, XMLEventReader]` for some effect type F.
  *
  * This typeclass has four main members:
  *  - `File` - constructs a `FileInputStream` which will automatically be closed
  *  - `String` - constructs a `StringReader` which will automatically be closed
  *  - `Resource[F, InputStream]` - directly references the stream, and delegates to the resource's own close logic
  *  - `Resource[F, Reader]` - directly references the reader, and delegates to the resource's own close logic
  *
  * Since the `R` type is contravariant, you may also use subtypes of `InputStream` and `Reader`, e.g. `FilterInputStream` or `BufferedReader`,
  * as long as they are wrapped in a `Resource` to manage their close logic.
  *
  * @group support
  */
trait IntoXmlEventReader[F[_], -R] {
	def apply(factory: XMLInputFactory, source: R)(implicit F: Sync[F]): Resource[F, XMLEventReader]
}

/**
  * @group support
  */
object IntoXmlEventReader {

	/** Conversion for `Resource[F, java.io.InputStream]`, for any Applicative type-constructor `F`.
	  */
	implicit def forInputStreamResource[F[_]: Applicative](implicit codec: Codec = null): IntoXmlEventReader[F, Resource[F, InputStream]] = new IntoXmlEventReader[F, Resource[F, InputStream]] {
		def apply(factory: XMLInputFactory, source: Resource[F, InputStream])(implicit F: Sync[F]): Resource[F, XMLEventReader] = source.map { closeableStream =>
			val guardedStream = new FilterInputStream(closeableStream) { override def close() = () }

			if(codec == null) factory.createXMLEventReader(guardedStream)
			else factory.createXMLEventReader(guardedStream, codec.name)
		}
	}

	/** Conversion for `Resource[F, java.io.Reader]`, for any Applicative type-constructor `F`.
	  */
	implicit def forReaderResource[F[_]: Applicative]: IntoXmlEventReader[F, Resource[F, Reader]] = new IntoXmlEventReader[F, Resource[F, Reader]] {
		def apply(factory: XMLInputFactory, source: Resource[F, Reader])(implicit F: Sync[F]): Resource[F, XMLEventReader] = source.map { closeableReader =>
			val guardedReader = new FilterReader(closeableReader) { override def close() = () }
			factory.createXMLEventReader(guardedReader)
		}
	}

	/** Conversion for `String`, for any type-constructor `F`
	  */
	implicit def forRawXmlString[F[_]]: IntoXmlEventReader[F, String] = new IntoXmlEventReader[F, String] {
		def apply(factory: XMLInputFactory, source: String)(implicit F: Sync[F]): Resource[F, XMLEventReader] = {
			val acquire = F.delay { factory.createXMLEventReader(new StringReader(source)) }
			Resource.make(acquire)(stream => F.delay { stream.close() })
		}
	}

	/** Conversion for `java.io.File`, for any type-constructor `F`
	  */
	implicit def forFile[F[_]](implicit codec: Codec = null): IntoXmlEventReader[F, File] = new IntoXmlEventReader[F, File] {
		def apply(factory: XMLInputFactory, source: File)(implicit F: Sync[F]): Resource[F, XMLEventReader] = {
			val contentsResource = Resource.fromAutoCloseable(Sync[F].delay { new FileInputStream(source) })
			forInputStreamResource[F].apply(factory, contentsResource)
		}
	}

	/** Non-conversion for an existing `Resource[F, XMLEventReader]`
	  */
	implicit def identity[F[_]]: IntoXmlEventReader[F, Resource[F, XMLEventReader]] = new IntoXmlEventReader[F, Resource[F, XMLEventReader]] {
		def apply(factory: XMLInputFactory, source: Resource[F, XMLEventReader])(implicit F: Sync[F]) = source
	}
}