package io.dylemma.spac
package json

import com.fasterxml.jackson.core.{JsonFactory, JsonParser => JacksonParser}
import io.dylemma.spac.json.impl.JsonStackFixer

import java.io.{File, InputStream, Reader}

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

	/** Wrap an existing Jackson JsonParser as a single-use source of JsonEvents.
	  *
	  * The source will *not* attempt to close the underlying parser.
	  *
	  * @param jacksonParser A Jackson JsonParser
	  * @return A single-use JSON event source
	  */
	def apply(jacksonParser: JacksonParser): Source[JsonEvent] = {
		Source.singleUse(JsonStackFixer.transform(new WrappedJacksonParser(jacksonParser)))
	}

	/** Wrap a string of raw JSON as a source of JsonEvents
	  *
	  * @param rawJson A string containing raw JSON
	  * @param factory A Jackson JsonFactory used to construct the underlying JsonParser for the string
	  * @return A reusable JSON event source
	  */
	def fromString(rawJson: String, factory: JsonFactory = defaultFactory): Source[JsonEvent] = Source.defer {
		apply(factory.createParser(rawJson).disable(JacksonParser.Feature.AUTO_CLOSE_SOURCE))
	}

	/** Wrap a `Reader` as a single-use source of JsonEvents.
	  *
	  * The Source will *not* attempt to close the reader.
	  *
	  * @param reader A Reader over characters of raw JSON
	  * @param factory A Jackson JsonFactory used to construct the underlying JsonParser for the string
	  * @return A single-use JSON event source
	  */
	def fromReader(reader: Reader, factory: JsonFactory = defaultFactory): Source[JsonEvent] = {
		apply(factory.createParser(reader).disable(JacksonParser.Feature.AUTO_CLOSE_SOURCE))
	}

	/** Wrap an InputStream as a single-use source of JsonEvents.
	  *
	  * The Source will *not* attempt to close the input stream.
	  *
	  * @param stream An InputStream over raw JSON bytes
	  * @param factory A Jackson JsonFactory used to construct the underlying JsonParser for the string
	  * @return A single-use JSON event source
	  */
	def fromInputStream(stream: InputStream, factory: JsonFactory = defaultFactory): Source[JsonEvent] = {
		apply(factory.createParser(stream).disable(JacksonParser.Feature.AUTO_CLOSE_SOURCE))
	}

	/** Creates a new Source that reads JsonEvents from the given file.
	  *
	  * @param file A JSON file
	  * @param factory A Jackson JsonFactory used to construct the underlying JsonParser for the string
	  * @return A reusable JSON event source
	  */
	def fromFile(file: File, factory: JsonFactory = defaultFactory): Source[JsonEvent] = () => {
		val parser = factory.createParser(file)
		val (itr, _) = apply(parser).open() // underlying source's close is a no-op, so don't bother capturing it
		val close = () => parser.close()
		itr -> close
	}

}
