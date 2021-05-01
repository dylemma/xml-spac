package io.dylemma.spac
package json

import java.io.{File, InputStream, Reader}

import cats.effect.{Resource, Sync}
import com.fasterxml.jackson.core.{JsonFactory, JsonParser => JacksonParser}

/** Typeclass for "source" types that can be passed to a `com.fasterxml.jackson.core.JsonFactory`
  * to create a `com.fasterxml.jackson.core.JsonParser` (wrapped as a Resource) which can be
  * used as the underlying JsonEvent source for spac JsonParsers.
  *
  * @tparam F An effect type, e.g. `cats.effect.IO` used to represent the opening and closing of the underlying parser
  * @tparam S The source type, e.g. `File`, `String`, `Resource[F, Reader]`, or `Resource[F, InputStream]`
  * @group support
  */
trait IntoJacksonJsonParser[F[_], -S] {
	def apply(factory: JsonFactory, source: S)(implicit F: Sync[F]): Resource[F, JacksonParser]
}
/** @group support */
object IntoJacksonJsonParser {
	implicit def forFile[F[_]]: IntoJacksonJsonParser[F, File] = new IntoJacksonJsonParser[F, File] {
		def apply(factory: JsonFactory, source: File)(implicit F: Sync[F]) = {
			Resource.fromAutoCloseable(F.delay {factory.createParser(source)})
		}
	}

	implicit def forRawJsonString[F[_]]: IntoJacksonJsonParser[F, String] = new IntoJacksonJsonParser[F, String] {
		def apply(factory: JsonFactory, source: String)(implicit F: Sync[F]) = {
			Resource.fromAutoCloseable(F.delay {factory.createParser(source)})
		}
	}

	implicit def forReaderResource[F[_]]: IntoJacksonJsonParser[F, Resource[F, Reader]] = new IntoJacksonJsonParser[F, Resource[F, Reader]] {
		def apply(factory: JsonFactory, source: Resource[F, Reader])(implicit F: Sync[F]) = {
			source.map { reader =>
				factory.createParser(reader).disable(JacksonParser.Feature.AUTO_CLOSE_SOURCE)
			}
		}
	}

	implicit def forInputStreamResource[F[_]]: IntoJacksonJsonParser[F, Resource[F, InputStream]] = new IntoJacksonJsonParser[F, Resource[F, InputStream]] {
		def apply(factory: JsonFactory, source: Resource[F, InputStream])(implicit F: Sync[F]) = {
			source.map { stream =>
				factory.createParser(stream).disable(JacksonParser.Feature.AUTO_CLOSE_SOURCE)
			}
		}
	}

	implicit def identity[F[_]]: IntoJacksonJsonParser[F, Resource[F, JacksonParser]] = new IntoJacksonJsonParser[F, Resource[F, JacksonParser]] {
		def apply(factory: JsonFactory, source: Resource[F, JacksonParser])(implicit F: Sync[F]) = source
	}
}
