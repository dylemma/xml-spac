package io.dylemma.spac.example

import cats.effect.SyncIO
import cats.effect.kernel.Resource
import fs2._
import io.dylemma.spac._
import io.dylemma.spac.xml._

import java.io.StringReader
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

	// This won't compile without a "support" import to provide the underlying
	// conversion from a String to a series of XmlEvent.
	/* parser.parse(rawXml) */

	/* The previous examples all use "JavaxSupport" in their imports,
	 * which provides implicits that cause an XmlParser's `parse` method
	 * to plug the input into a javax.xml stream reader to use as the
	 * underlying event source.
	 *
	 * But xml-spac is generic with respect to "event providers".
	 * You can instead use the `Fs2DataSupport` object to wire the `parse`
	 * method up to use fs2-data-xml as the underlying event source.
	 */
	locally {
		import Fs2DataSupport._
		parser.parse(rawXml)

		// Fs2DataSupport doesn't add support for java.io classes, so this won't compile:
		/* parser parse Resource.fromAutoCloseable(SyncIO {new StringReader(rawXml)}) */

		// Fs2DataSupport *does* add support for Streams of "raw xml", e.g. Char/String.
		// Note that if the stream's effect type was anything but `Pure`, you would need to use `parseF` instead of `parse`
		val parsedFromStream = parser parse Stream.emits(rawXml)
		println(s"Parsed From Stream: $parsedFromStream")

		// If you want to parse a stream of Byte values with fs2-data-xml, you need to pick an encoding
		// and import the corresponding object from `fs2.data.text`.
		// This will provide the appropriate `CharLikeChunks` instance for `Byte`
		// so the underlying parser pipe can be created.
		locally {
			val rawXmlBytes = rawXml.getBytes(Charset.forName("ISO-8859-1"))
			val rawXmlByteStream = Stream.emits(rawXmlBytes)

			import fs2.data.text.latin1._
			val parsedFromBytes = parser.parse(rawXmlByteStream)
			println(s"Parsed From Bytes:  $parsedFromBytes")
		}
	}

	locally {
		import JavaxSupport._
		parser.parse(rawXml)

		val parsedFromReader = parser parse Resource.fromAutoCloseable(SyncIO {new StringReader(rawXml)})
		println(s"Parsed From Reader: $parsedFromReader")

		// JavaxSupport is focused on java.io-based sources,
		// so it doesn't provide support for fs2.Streams.
		// This won't compile:
		/* parser parseF Stream.emits(rawXml) */
	}
}
