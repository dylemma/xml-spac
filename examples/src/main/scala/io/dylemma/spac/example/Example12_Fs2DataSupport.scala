package io.dylemma.spac.example

import cats.effect.SyncIO
import fs2._
import io.dylemma.spac._
import io.dylemma.spac.interop.fs2._
import io.dylemma.spac.xml._

import java.nio.charset.Charset

object Example12_Fs2DataSupport extends App {

	val rawXml =
		"""<library>
		  | <book>Don Quixote</book>
		  | <book>A Tale of Two Cities</book>
		  | <book>The Lord of the Rings</book>
		  | <book>The Little Prince</book>
		  | <book>Harry Potter and the Sorcerer's Stone</book>
		  | <book>And Then There Were None</book>
		  |</library>""".stripMargin

	val parser = Splitter.xml("library" \ "book").text.parseToList

	/* The previous examples all use "JavaxSource" to parse the raw data into XmlEvents.
	 * This one uses `Fs2DataSource`, which uses the `fs2-data-xml` library under the hood.
	 * Unlike `JavaxSource` which provides a `Source[XmlEvent]`, the `Fs2DataSource` helpers
	 * create `fs2.Stream[F, XmlEvent]` for an `F` effect type of your choice.
	 */
	val xmlEventsFromString: fs2.Stream[SyncIO, XmlEvent] = Fs2DataSource.fromString[SyncIO](rawXml)
	val xmlEventsFromBytes: fs2.Stream[SyncIO, XmlEvent] = locally {
		// If you want to parse a stream of Byte values with fs2-data-xml, you need to pick an encoding
		// and import the corresponding object from `fs2.data.text`.
		// This will provide the appropriate `CharLikeChunks` instance for `Byte`
		// so the underlying parser pipe can be created.
		import fs2.data.text.latin1._
		val xmlBytes = rawXml.getBytes(Charset.forName("ISO-8859-1"))
		Fs2DataSource.fromRawXmlStream(Stream.emits[SyncIO, Byte](xmlBytes))
	}

	/* A Parser's `parse` method is intended for synchronous processing, consuming the
	 * whole source and returning a result before `parse` is finished.
	 * This isn't friendly for fs2 and possibly-asynchronous streams, so we use
	 * `import io.dylemma.spac.interop.fs2._` to add the `parseF` method to Parsers.
	 * This allows a `Parser[In, Out]` to consume a `fs2.Stream[F, In]` as an `F[Out]`.
	 * E.g. an `XmlParser[A]` can consume a `fs2.Stream[F, XmlEvent]` from Fs2DataSource
	 *  as an `F[A]`.
	 */
	locally {

		// Under the hood, `parser.parseF(stream)` uses `stream.through(parser.toPipe).compile.lastOrError`,
		// so the return type is *usually* `F[Out]` but it can be different if the implicit `Compiler` wants.
		val parsedFromStream = parser.parseF(xmlEventsFromString).unsafeRunSync()
		println(s"Parsed From Stream: $parsedFromStream")

		val parsedFromBytes = parser.parseF(xmlEventsFromBytes).unsafeRunSync()
		println(s"Parsed From Bytes:  $parsedFromBytes")
	}

}
