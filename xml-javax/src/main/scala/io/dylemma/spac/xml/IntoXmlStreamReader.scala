package io.dylemma.spac
package xml

import java.io._

import cats.Applicative
import cats.effect.{Resource, Sync}
import javax.xml.stream.{XMLInputFactory, XMLStreamReader}

import scala.io.Codec

/** Typeclass representing the conversion of some `source` into a javax `XMLStreamReader`.
  *
  * The opening and closing of underlying resources like file/stream handles is represented with `cats.effect.Resource`,
  * where the result of "converting" a source is a `Resource[F, XMLStreamReader]` for some effect type F.
  *
  * This typeclass has four main members:
  *  - `File` - constructs a `FileInputStream` which will automatically be closed
  *  - `String` - constructs a `StringReader` which will automatically be closed
  *  - `Resource[F, InputStream]` - directly references the stream, and delegates to the resource's own close logic
  *  - `Resource[F, Reader]` - directly references the reader, and delegates to the resource's own close logic
  *
  * Since the `R` type is contravariant, you may also use subtypes of `InputStream` and `Reader`, e.g. `FilterInputStream` or `BufferedReader`,
  * as long as they are wrapped in a `Resource` to manage their close logic.
  */
trait IntoXmlStreamReader[F[_], -R] {
	def apply(factory: XMLInputFactory, source: R)(implicit F: Sync[F]): Resource[F, XMLStreamReader]
}
object IntoXmlStreamReader {

	/** Conversion for `Resource[F, java.io.InputStream]`, for any Applicative type-constructor `F`.
 	  */
	implicit def forInputStreamResource[F[_]: Applicative](implicit codec: Codec = null): IntoXmlStreamReader[F, Resource[F, InputStream]] = new IntoXmlStreamReader[F, Resource[F, InputStream]] {
		def apply(factory: XMLInputFactory, source: Resource[F, InputStream])(implicit F: Sync[F]): Resource[F, XMLStreamReader] = source.map { closeableStream =>
			val guardedStream = new FilterInputStream(closeableStream) { override def close() = () }

			if(codec == null) factory.createXMLStreamReader(guardedStream)
			else factory.createXMLStreamReader(guardedStream, codec.name)
		}
	}

	/** Conversion for `Resource[F, java.io.Reader]`, for any Applicative type-constructor `F`.
	  */
	implicit def forReaderResource[F[_]: Applicative]: IntoXmlStreamReader[F, Resource[F, Reader]] = new IntoXmlStreamReader[F, Resource[F, Reader]] {
		def apply(factory: XMLInputFactory, source: Resource[F, Reader])(implicit F: Sync[F]): Resource[F, XMLStreamReader] = source.map { closeableReader =>
			val guardedReader = new FilterReader(closeableReader) { override def close() = () }
			factory.createXMLStreamReader(guardedReader)
		}
	}

	/** Conversion for `String`, for any type-constructor `F`
	  */
	implicit def forRawXmlString[F[_]]: IntoXmlStreamReader[F, String] = new IntoXmlStreamReader[F, String] {
		def apply(factory: XMLInputFactory, source: String)(implicit F: Sync[F]): Resource[F, XMLStreamReader] = {
			val acquire = F.delay { factory.createXMLStreamReader(new StringReader(source)) }
			Resource.make(acquire)(stream => F.delay { stream.close() })
		}
	}

	/** Conversion for `java.io.File`, for any type-constructor `F`
	  */
	implicit def forFile[F[_]](implicit codec: Codec = null): IntoXmlStreamReader[F, File] = new IntoXmlStreamReader[F, File] {
		def apply(factory: XMLInputFactory, source: File)(implicit F: Sync[F]): Resource[F, XMLStreamReader] = {
			val contentsResource = Resource.fromAutoCloseable(Sync[F].delay { new FileInputStream(source) })
			forInputStreamResource[F].apply(factory, contentsResource)
		}
	}

	implicit def identity[F[_]]: IntoXmlStreamReader[F, Resource[F, XMLStreamReader]] = new IntoXmlStreamReader[F, Resource[F, XMLStreamReader]] {
		def apply(factory: XMLInputFactory, source: Resource[F, XMLStreamReader])(implicit F: Sync[F]) = source
	}
}