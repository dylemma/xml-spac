package io.dylemma.spac.xml2

import java.io._

import cats.Applicative
import cats.effect.{Resource, Sync}
import javax.xml.stream.{XMLInputFactory, XMLStreamReader}

import scala.io.Codec

trait IntoXmlStreamReader[F[_], -R] {
	def apply(factory: XMLInputFactory, source: R)(implicit F: Sync[F]): Resource[F, XMLStreamReader]
}
object IntoXmlStreamReader {

	// Resource[F, java.io.InputStream]
	implicit def forInputStreamResource[F[_]: Applicative](implicit codec: Codec = null): IntoXmlStreamReader[F, Resource[F, InputStream]] = new IntoXmlStreamReader[F, Resource[F, InputStream]] {
		def apply(factory: XMLInputFactory, source: Resource[F, InputStream])(implicit F: Sync[F]): Resource[F, XMLStreamReader] = source.map { closeableStream =>
			val guardedStream = new FilterInputStream(closeableStream) { override def close() = () }

			if(codec == null) factory.createXMLStreamReader(guardedStream)
			else factory.createXMLStreamReader(guardedStream, codec.name)
		}
	}

	// Resource[F, java.io.Reader]
	implicit def forReaderResource[F[_]: Applicative]: IntoXmlStreamReader[F, Resource[F, Reader]] = new IntoXmlStreamReader[F, Resource[F, Reader]] {
		def apply(factory: XMLInputFactory, source: Resource[F, Reader])(implicit F: Sync[F]): Resource[F, XMLStreamReader] = source.map { closeableReader =>
			val guardedReader = new FilterReader(closeableReader) { override def close() = () }
			factory.createXMLStreamReader(guardedReader)
		}
	}

	// String
	implicit def forRawXmlString[F[_]]: IntoXmlStreamReader[F, String] = new IntoXmlStreamReader[F, String] {
		def apply(factory: XMLInputFactory, source: String)(implicit F: Sync[F]): Resource[F, XMLStreamReader] = {
			val acquire = F.delay { factory.createXMLStreamReader(new StringReader(source)) }
			Resource.make(acquire)(stream => F.delay { stream.close() })
		}
	}

	// File
	implicit def forFile[F[_]](implicit codec: Codec = null): IntoXmlStreamReader[F, File] = new IntoXmlStreamReader[F, File] {
		def apply(factory: XMLInputFactory, source: File)(implicit F: Sync[F]): Resource[F, XMLStreamReader] = {
			val contentsResource = Resource.fromAutoCloseable(Sync[F].delay { new FileInputStream(source) })
			forInputStreamResource[F].apply(factory, contentsResource)
		}
	}
}