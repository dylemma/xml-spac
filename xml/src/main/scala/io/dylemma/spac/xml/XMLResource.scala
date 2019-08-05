package io.dylemma.spac.xml

import java.io.{File, FileInputStream, InputStream, StringReader}
import javax.xml.stream.events.XMLEvent
import javax.xml.stream.{XMLEventReader, XMLInputFactory}

import io.dylemma.spac.{ConsumableLike, Handler}

import scala.util.control.NonFatal

trait XMLResource[T] {
	type Opened

	def open(from: T): Opened
	def close(resource: Opened): Unit
	def getReader(factory: XMLInputFactory, resource: Opened): XMLEventReader
}

object XMLResource {

	implicit def consumableLike[T: XMLResource]: ConsumableLike[T, XMLEvent] = new ConsumableLike[T, XMLEvent] {
		def getIterator(resource: T): Iterator[XMLEvent] with AutoCloseable = {
			XMLEvents(resource).iterator
		}
		def apply[R](source: T, handler: Handler[XMLEvent, R]): R = {
			runIterator(XMLEvents(source).iterator, handler)
		}
	}

	implicit object FileXMLResource extends XMLResource[File] {
		type Opened = FileInputStream
		def close(resource: FileInputStream) = {
			try resource.close() catch {case NonFatal(_) => ()}
		}
		def getReader(factory: XMLInputFactory, resource: FileInputStream) = {
			factory.createXMLEventReader(resource)
		}
		def open(from: File) = new FileInputStream(from)
	}

	implicit object RawXMLResource extends XMLResource[String] {
		type Opened = StringReader
		def open(s: String) = new StringReader(s)
		def close(reader: StringReader) = reader.close()
		def getReader(factory: XMLInputFactory, reader: StringReader) = {
			factory.createXMLEventReader(reader)
		}
	}
	
	implicit object InputStreamReaderXMLResource extends XMLResource[BufferedSource] {
    type Opened = InputStreamReader
    def open(source: BufferedSource): InputStreamReader = try source.reader() catch {
      case npe: NullPointerException if npe.getMessage == "charset decoder" => throw npe
      case _: NullPointerException => throw new FileNotFoundException("File specified as input stream was null")
    }
    def close(reader: InputStreamReader): Unit = reader.close()
    def getReader(factory: XMLInputFactory, reader: InputStreamReader): XMLEventReader = {
      factory.createXMLEventReader(reader)
    }
  }
	
	implicit object InputStreamXMLResource extends XMLResource[InputStreamReader] {
    type Opened = InputStreamReader
    def open(stream: InputStreamReader) = stream
    def close(stream: InputStreamReader): Unit = {
      try stream.close() catch {case NonFatal(_) => ()}
    }
    def getReader(factory: XMLInputFactory, stream: InputStreamReader) = {
      factory.createXMLEventReader(stream)
    }
  }

	implicit object InputStreamXMLResource extends XMLResource[InputStream] {
		type Opened = InputStream
		def open(stream: InputStream) = stream
		def close(stream: InputStream): Unit = {
			try stream.close() catch {case NonFatal(_) => ()}
		}
		def getReader(factory: XMLInputFactory, stream: InputStream) = {
			factory.createXMLEventReader(stream)
		}
	}

	implicit def getXMLResourceForFactory[T: XMLResource] = new ResourceFactoryXMLResource[T]
	class ResourceFactoryXMLResource[T: XMLResource] extends XMLResource[() => T] {
		val delegate = implicitly[XMLResource[T]]
		type Opened = delegate.Opened
		def open(factory: () => T) = delegate.open(factory())
		def close(resource: delegate.Opened) = delegate.close(resource)
		def getReader(factory: XMLInputFactory, resource: delegate.Opened) = {
			delegate.getReader(factory, resource)
		}
	}
}
