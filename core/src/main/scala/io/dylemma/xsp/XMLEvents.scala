package io.dylemma.xsp

import java.io.Closeable
import java.util.concurrent.atomic.AtomicBoolean
import javax.xml.stream.events.XMLEvent
import javax.xml.stream.{XMLEventReader, XMLInputFactory}

import scala.util.control.NonFatal

object XMLEvents {
	lazy val defaultFactory: XMLInputFactory = {
		val factory = XMLInputFactory.newInstance
		factory.setProperty(XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES, false)
		factory.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false)
		factory
	}

	private sealed trait Resource {
		type Source
		def source: Source
		def provider: XMLResource[Source]
		def factory: XMLInputFactory
	}
	private object Resource {
		def apply[T: XMLResource](s: T, f: XMLInputFactory) = new Resource {
			type Source = T
			val source = s
			val provider = implicitly[XMLResource[T]]
			val factory = f
		}
	}

	def apply[T: XMLResource](source: T, factory: XMLInputFactory = defaultFactory) = {
		new XMLEvents(Resource(source, factory))
	}
}

class XMLEvents private(r: XMLEvents.Resource) {

	private val source = r.source
	private val factory = r.factory
	private val provider = r.provider

	/** Opens an XML event reader from the `resource` provided in the constructor
		* @return The reader, paired with a function that should be called when
		*         done with the reader
		*/
	def open: (XMLEventReader, () => Unit) = {
		val opened = provider.open(source)
		val reader =
			try provider.getReader(factory, opened)
			catch {
				case e@NonFatal(_) =>
					provider.close(opened)
					throw e
			}
		val didClose = new AtomicBoolean(false)
		val closeFunc = () => {
			if(didClose.compareAndSet(false, true)){
				provider.close(opened)
			}
		}
		reader -> closeFunc
	}

	def iterator: Iterator[XMLEvent] with Closeable = {
		val (reader, closeFunc) = open
		new Iterator[XMLEvent] with Closeable {
			def hasNext: Boolean = reader.hasNext
			def next(): XMLEvent = reader.nextEvent
			def close(): Unit = closeFunc()
		}
	}

	def feedTo[Out](consumer: Consumer[XMLEvent, Out]) = {
		val (reader, closeFunc) = open
		var result: Option[Out] = None
		try {
			val handler = consumer.makeHandler()
			while(reader.hasNext && !handler.isFinished){
				result = handler.handleInput(reader.nextEvent())
			}
			result getOrElse handler.handleEnd()
		} finally {
			closeFunc()
		}
	}
}