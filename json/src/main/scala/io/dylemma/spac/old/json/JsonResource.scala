package io.dylemma.spac.old.json

import java.io.{File, InputStream, Reader}

import com.fasterxml.jackson.core.{JsonFactory, JsonParser => JacksonParser}
import io.dylemma.spac.old.Handler
import io.dylemma.spac.old.{ConsumableLike, Handler}

trait JsonResource[-R] {
	def createParser(factory: JsonFactory, resource: R): JacksonParser
}

object JsonResource {
	implicit def consumableLike[T: JsonResource]: ConsumableLike[T, JsonEvent] = new ConsumableLike[T, JsonEvent] {

		def getIterator(resource: T): Iterator[JsonEvent] with AutoCloseable = JsonEvents(resource).iterator
		def apply[A](resource: T, handler: Handler[JsonEvent, A]) = {
			runIterator(JsonEvents(resource).iterator, handler)
		}
	}

	def apply[T](f: (JsonFactory, T) => JacksonParser): JsonResource[T] = new JsonResource[T] {
		def createParser(factory: JsonFactory, resource: T): JacksonParser = f(factory, resource)
	}
	implicit val fileResource: JsonResource[File] = JsonResource[File](_ createParser _)

	/** JsonResource for InputStream.
	  * Will *NOT* auto-close the stream; the responsibility for closing the stream lies with whoever created it.
	  */
	implicit val inputStreamResource: JsonResource[InputStream] = JsonResource[InputStream](_.createParser(_).disable(JacksonParser.Feature.AUTO_CLOSE_SOURCE))
	/** JsonResource for Reader.
	  * Will *NOT* auto-close the reader; the responsibility for closing the reader lies with whoever created it.
	  */
	implicit val readerResource: JsonResource[Reader] = JsonResource[Reader](_.createParser(_).disable(JacksonParser.Feature.AUTO_CLOSE_SOURCE))

	implicit val stringResource: JsonResource[String] = JsonResource[String](_ createParser _)

	/** JsonResource for "constructors" of types that belong to the JsonResource typeclass.
	  * This method can be used to indirectly support types that don't belong to the JsonResource typeclass.
	  *
	  * Streams and Readers constructed by the returned JsonResource *WILL* be auto-closed,
	  * as the responsibility for closing the stream/reader lies with whoever created it,
	  * which is this JsonResource.
	  */
	implicit def constructedResource[T: JsonResource]: JsonResource[() => T] = new JsonResource[() => T] {
		def createParser(factory: JsonFactory, constructor: () => T): JacksonParser = {
			implicitly[JsonResource[T]].createParser(factory, constructor()).enable(JacksonParser.Feature.AUTO_CLOSE_SOURCE)
		}
	}
}
