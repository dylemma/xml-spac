package io.dylemma.spac.xml

import java.io._

import io.dylemma.spac.{ConsumableLike, Handler}
import javax.xml.stream.events.XMLEvent
import javax.xml.stream.{XMLEventReader, XMLInputFactory}

import scala.util.control.NonFatal

trait XMLResource[-T] {
	type Opened

	def open(from: T): Opened
	def close(resource: Opened): Unit
	def getReader(factory: XMLInputFactory, resource: Opened): XMLEventReader
}

object XMLResource {

	/** Provides a `ConsumableLike` instance for any type `T` belonging to the `XMLResource` typeclass.
	  *
	  * @tparam T A type representing some source of XML data
	  * @return A `ConsumableLike[T, XMLEvent]`
	  */
	implicit def consumableLike[T: XMLResource]: ConsumableLike[T, XMLEvent] = new ConsumableLike[T, XMLEvent] {
		def getIterator(resource: T): Iterator[XMLEvent] with AutoCloseable = {
			XMLEvents(resource).iterator
		}
		def apply[R](source: T, handler: Handler[XMLEvent, R]): R = {
			runIterator(XMLEvents(source).iterator, handler)
		}
	}

	/** Creates an XMLEventReader by opening a `FileInputStream` of the given `File`.
	  * The opened FileInputStream will be closed by passing it to this object's `close` method.
	  */
	implicit object FileXMLResource extends XMLResource[File] {
		type Opened = FileInputStream
		def close(resource: FileInputStream) = closeQuietly(resource)
		def getReader(factory: XMLInputFactory, resource: FileInputStream) = {
			factory.createXMLEventReader(resource)
		}
		def open(from: File) = new FileInputStream(from)
	}

	/** Creates an XMLEventReader from a given `String` whose contents are assumed to be XML.
	  * Under the hood, a StreamReader is opened and closed by this object.
	  */
	implicit object RawXMLResource extends XMLResource[String] {
		type Opened = StringReader
		def open(s: String) = new StringReader(s)
		def close(reader: StringReader) = reader.close()
		def getReader(factory: XMLInputFactory, reader: StringReader) = {
			factory.createXMLEventReader(reader)
		}
	}

	/** Creates an XMLEventReader from a given `java.io.InputStream`.
	  * This object does not attempt to close the given `InputStream`.
	  */
	implicit object InputStreamXMLResource extends XMLResource[InputStream] {
		type Opened = InputStream
		def open(stream: InputStream): Opened = stream
		def close(stream: Opened): Unit = ()
		def getReader(factory: XMLInputFactory, stream: InputStream) = {
			factory.createXMLEventReader(stream)
		}
	}

	/** Creates an XMLEventReader from a given `java.io.Reader`.
	  * This object does not attempt to close the given `Reader`.
	  */
	implicit object ReaderXMLResource extends XMLResource[Reader] {
		type Opened = Reader
		def open(from: Reader): Opened = from
		def close(resource: Opened): Unit = ()
		def getReader(factory: XMLInputFactory, reader: Opened): XMLEventReader = {
			factory.createXMLEventReader(reader)
		}
	}


	implicit def getXMLResourceForFactory[T: XMLResource] = new ResourceFactoryXMLResource[T]
	class ResourceFactoryXMLResource[T: XMLResource] extends XMLResource[() => T] {
		val delegate = implicitly[XMLResource[T]]
		type Opened = delegate.Opened
		def open(factory: () => T) = delegate.open(factory())
		def close(resource: delegate.Opened) = {
			// since this object was responsible for creating the `T` instance,
			// it is also responsible for closing it, if `T <: AutoCloseable`
			resource match {
				case r: AutoCloseable => closeQuietly(r)
				case _ =>
			}
			delegate.close(resource)
		}
		def getReader(factory: XMLInputFactory, resource: delegate.Opened) = {
			delegate.getReader(factory, resource)
		}
	}

	private def closeQuietly(r: AutoCloseable): Unit = {
		try r.close() catch { case NonFatal(_) => () }
	}
}