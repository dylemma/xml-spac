package io.dylemma.spac.xml

import io.dylemma.spac.Source

import java.io._
import javax.xml.stream.{XMLEventReader, XMLInputFactory}

/** Provides helpers for creating `Source[XmlEvent]` using `javax.xml.stream`
  * for the underlying event provider.
  *
  * @group support
  */
object JavaxSource {
	/** Default `XMLInputFactory` used when creating an underlying `XMLEventReader`
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

	/** Returns a single-use `Source[XmlEvent]` which interprets the contents of the given `InputStream` as raw XML bytes.
	  *
	  * The returned `Source` will *not* attempt to close the `rawXml` stream;
	  * responsibility for closing `rawXml` lies with whoever created it.
	  *
	  * @param rawXml  An InputStream containing raw XML bytes
	  * @param factory Factory instance for the underlying Javax parser
	  * @return A single-use `Source[XmlEvent]`
	  */
	def fromInputStream(rawXml: InputStream, factory: XMLInputFactory = defaultFactory): Source[XmlEvent] = Source.deferOnce {
		val guardedStream = new FilterInputStream(rawXml) {
			override def close() = ()
		}
		val eventReader = factory.createXMLEventReader(guardedStream)
		apply(eventReader)
	}

	/** Returns a single-use `Source[XmlEvent]` which interprets the contents of the given `InputStream` as raw XML bytes,
	  * passing the given charset to the underlying XMLEventReader constructor
	  *
	  * The returned `Source` will *not* attempt to close the `rawXml` stream;
	  * responsibility for closing `rawXml` lies with whoever created it.
	  *
	  * @param rawXml  An InputStream containing raw XML bytes
	  * @param charset Name of the charset used to interpret the bytes to characters.
	  * @param factory Factory instance for the underlying Javax parser
	  * @return A single-use `Source[XmlEvent]`
	  */
	def fromInputStreamWithCharset(rawXml: InputStream, charset: String, factory: XMLInputFactory = defaultFactory): Source[XmlEvent] = Source.deferOnce {
		val guardedStream = new FilterInputStream(rawXml) {
			override def close() = ()
		}
		val eventReader = factory.createXMLEventReader(guardedStream, charset)
		apply(eventReader)
	}

	/** Returns a single-use `Source[XmlEvent]` which interprets the contents of the given `Reader` as raw XML characters.
	  *
	  * The returned `Source` will *not* attempt to close the `rawXml` reader;
	  * responsibility for closing `rawXml` lies with whoever created it.
	  *
	  * @param rawXml  A Reader containing raw XML character data
	  * @param factory Factory instance for the underlying Javax parser
	  * @return A single-use `Source[XmlEvent]`
	  */
	def fromReader(rawXml: Reader, factory: XMLInputFactory = defaultFactory): Source[XmlEvent] = Source.deferOnce {
		val guardedReader = new FilterReader(rawXml) {
			override def close() = ()
		}
		val eventReader = factory.createXMLEventReader(guardedReader)
		apply(eventReader)
	}

	/** Returns a `Source[XmlEvent]` which can open the given file to read raw XML data.
	  *
	  * The returned `Source` is reusable. The underlying streams are managed by the `open`
	  * method and the `close` function it returns.
	  *
	  * @param file    A file containing XML
	  * @param factory Factory instance for the underlying Javax parser
	  * @return A reusable `Source[XmlEvent]`
	  */
	def fromFile(file: File, factory: XMLInputFactory = defaultFactory): Source[XmlEvent] = () => {
		val rawXml = new FileInputStream(file)
		val (itr, innerClose) = fromInputStream(rawXml, factory).open()
		val close = () => try innerClose() finally rawXml.close()
		itr -> close
	}

	/** Like `fromFile`, but passes an explicit `charset` to the underlying XmlEventReader constructor.
	  *
	  * @param file    A file containing XML
	  * @param charset A Charset name used to interpret the bytes to characters
	  * @param factory Factory instance for the underlying Javax parser
	  * @return
	  */
	def fromFileWithCharset(file: File, charset: String, factory: XMLInputFactory = defaultFactory): Source[XmlEvent] = () => {
		val rawXml = new FileInputStream(file)
		val (itr, innerClose) = fromInputStreamWithCharset(rawXml, charset, factory).open()
		val close = () => try innerClose() finally rawXml.close()
		itr -> close
	}

	/** Returns a `Source[XmlEvent]` which interprets the given string as raw XML.
	  *
	  * @param rawXml  A string of raw XML
	  * @param factory Factory instance for the underlying Javax parser
	  * @return A reusable `Source[XmlEvent]`
	  */
	def fromString(rawXml: String, factory: XMLInputFactory = defaultFactory): Source[XmlEvent] = () => {
		val eventReader = factory.createXMLEventReader(new StringReader(rawXml))
		val (itr, innerClose) = apply(eventReader).open()
		val close = () => try innerClose() finally eventReader.close()
		itr -> close
	}

	def apply(eventReader: XMLEventReader): Source[XmlEvent] = {
		Source.singleUse(new WrappedEventReader(eventReader))
	}
}