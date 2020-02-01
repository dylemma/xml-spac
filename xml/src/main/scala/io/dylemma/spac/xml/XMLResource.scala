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

	/** Utility mixin that moves the `close()` functionality to the `reallyClose` method,
	  * and overrides `close()` itself to no-op.
	  *
	  * Used for resources that take stream/reader instances directly, to enforce the
	  * policy of "whoever creates the stream is responsible for closing it".
	  *
	  * Unfortunately, simply implementing an `XMLResource[T]` to have a no-op `close`
	  * method is not sufficient, since the javax XMLEventReader will try to close
	  * the underlying stream when it reaches the end. By mixing this trait into
	  * a proxy stream and using that to construct the XMLEventReader, we can guarantee
	  * that the underlying stream will only be closed by whoever constructed the
	  * underlying stream.
	  */
	trait HiddenCloseable extends Closeable {
		abstract override def close(): Unit = ()
		def reallyClose(): Unit = super.close()
	}

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
		def close(reader: StringReader): Unit = reader.close()
		def getReader(factory: XMLInputFactory, reader: StringReader) = {
			factory.createXMLEventReader(reader)
		}
	}

	/** Creates an XMLEventReader from a given `java.io.InputStream`.
	  * This object does not attempt to close the given `InputStream`.
	  */
	implicit object InputStreamXMLResource extends XMLResource[InputStream] {
		type Opened = NoCloseInputStream
		def open(stream: InputStream): Opened = new NoCloseInputStream(stream)
		def close(stream: Opened): Unit = println(s"don't close this stream: $stream")
		def getReader(factory: XMLInputFactory, stream: Opened) = {
			factory.createXMLEventReader(stream)
		}

		class NoCloseInputStream(in: InputStream) extends FilterInputStream(in) with HiddenCloseable
	}

	/** Creates an XMLEventReader from a given `java.io.Reader`.
	  * This object does not attempt to close the given `Reader`.
	  */
	implicit object ReaderXMLResource extends XMLResource[Reader] {
		type Opened = NoCloseReader
		def open(from: Reader): Opened = new NoCloseReader(from)
		def close(resource: Opened): Unit = println(s"don't close this reader: $resource")
		def getReader(factory: XMLInputFactory, reader: Opened): XMLEventReader = {
			factory.createXMLEventReader(reader)
		}

		class NoCloseReader(reader: Reader) extends FilterReader(reader) with HiddenCloseable
	}


	implicit def getXMLResourceForFactory[T: XMLResource] = new ResourceFactoryXMLResource[T]
	class ResourceFactoryXMLResource[T: XMLResource] extends XMLResource[() => T] {
		val delegate = implicitly[XMLResource[T]]
		type Opened = delegate.Opened
		def open(factory: () => T): Opened = delegate.open(factory())
		def close(resource: delegate.Opened): Unit = {
			// since this object was responsible for creating the `T` instance,
			// it is also responsible for closing it, if `T <: AutoCloseable`,
			// or if the delegate resource was one that used `HiddenCloseable` to hide the default `close` method
			resource match {
				case r: HiddenCloseable => r.reallyClose()
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