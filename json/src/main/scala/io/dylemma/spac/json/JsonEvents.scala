package io.dylemma.spac.json

import java.io.Closeable

import com.fasterxml.jackson.core.{JsonFactory, JsonToken}
import io.dylemma.spac.{ConsumableLike, Consumer, Handler}

object JsonEvents {
	implicit val consumableLike: ConsumableLike[JsonEvents, JsonEvent] = new ConsumableLike[JsonEvents, JsonEvent] {
		def apply[A](resource: JsonEvents, handler: Handler[JsonEvent, A]) = {
			runIterator(resource.iterator, handler)
		}
	}

	lazy val defaultFactory = new JsonFactory

	private sealed trait Resource {
		type Source
		def source: Source
		def provider: JsonResource[Source]
		def factory: JsonFactory
	}
	private object Resource {
		def apply[R: JsonResource](s: R, f: JsonFactory) = new Resource {
			type Source = R
			val source = s
			val provider = implicitly[JsonResource[R]]
			val factory = f
		}
	}

	def apply[R: JsonResource](source: R, factory: JsonFactory = defaultFactory) = {
		new JsonEvents(Resource(source, factory))
	}
}

class JsonEvents private(r: JsonEvents.Resource) {
	private val source = r.source
	private val factory = r.factory
	private val provider = r.provider

	def feedTo[Out](consumer: Consumer[JsonEvent, Out]): Out = {
		val itr = iterator
		var result: Option[Out] = None
		try {
			val handler = consumer.makeHandler()
			while(itr.hasNext && !handler.isFinished){
				result = handler.handleInput(itr.next())
			}
			result getOrElse handler.handleEnd()
		} finally {
			itr.close()
		}
	}

	def iterator: Iterator[JsonEvent] with Closeable = {
		val parser = provider.createParser(factory, source)
		new Iterator[JsonEvent] with Closeable {
			private var currentEvent: Option[JsonEvent] = None
			private var bufferIsNext = false // if this is true, then `currentEvent` should be returned by next()

			def hasNext = {
				if(!bufferIsNext) advance()
				currentEvent.isDefined
			}

			def next() = {
				if(!bufferIsNext) advance()
				currentEvent match {
					case Some(e) =>
						// Since we are consuming the buffered event, reset the flag
						// so the state will `advance()` next time
						bufferIsNext = false // since we are consuming the value
						e
					case None =>
						throw new NoSuchElementException("JSON parser reached EOF")
				}
			}

			def close() = {
				parser.close()
			}

			private def advance(): Unit = {
				val token = parser.nextToken
				if(token == null){
					// update the state to signify EOF
					currentEvent = None
					bufferIsNext = true
				} else {
					// update the state to contain the next value returned by getNext
					currentEvent = Some(token match {
						case JsonToken.START_OBJECT => JsonEvent.ObjectStart
						case JsonToken.FIELD_NAME => JsonEvent.ObjectField(parser.getCurrentName)
						case JsonToken.END_OBJECT => JsonEvent.ObjectEnd
						case JsonToken.START_ARRAY => JsonEvent.ArrayStart
						case JsonToken.END_ARRAY => JsonEvent.ArrayEnd
						case JsonToken.VALUE_FALSE => JsonEvent.JBool(false)
						case JsonToken.VALUE_TRUE => JsonEvent.JBool(true)
						case JsonToken.VALUE_NUMBER_INT => JsonEvent.JLong(parser.getLongValue)
						case JsonToken.VALUE_NUMBER_FLOAT => JsonEvent.JDouble(parser.getDoubleValue)
						case JsonToken.VALUE_STRING => JsonEvent.JString(parser.getText)
						case JsonToken.VALUE_NULL => JsonEvent.JNull
						case t =>
							println(s"unhandled JsonToken: $t")
							JsonEvent.Unknown
					})
					bufferIsNext = true
				}
			}
		}
	}
}