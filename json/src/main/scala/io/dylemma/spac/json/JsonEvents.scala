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
			private var contextStack: List[JsonStackElem] = Nil
			private var buffer: List[JsonEvent] = Nil
			private var isEOF = false

			def hasNext = {
				if(buffer.isEmpty && !isEOF) advance()
				buffer.nonEmpty
			}

			def close() = {
				parser.close()
			}

			def next() = {
				if(buffer.isEmpty && !isEOF) advance()
				buffer match {
					case event :: remaining =>
						buffer = remaining
						contextStack = event match {
							case push: JsonStackElem => push :: contextStack
							case _: JsonStackPop => contextStack.tail
							case _ => contextStack
						}
						event
					case Nil => throw new NoSuchElementException("next() at EOF")
				}
			}

			private def advance(): Unit = {
				val token = parser.nextToken
				if(token == null){
					// update the state to signify EOF
					buffer = Nil
					isEOF = true
				} else {
					isEOF = false

					// create an event based on the parser's current "token"
					val event = token match {
						case JsonToken.START_OBJECT => JsonEvent.ObjectStart
						case JsonToken.FIELD_NAME => JsonEvent.FieldStart(parser.getCurrentName)
						case JsonToken.END_OBJECT => JsonEvent.ObjectEnd
						case JsonToken.START_ARRAY => JsonEvent.ArrayStart
						case JsonToken.END_ARRAY => JsonEvent.ArrayEnd
						case JsonToken.VALUE_FALSE => JsonEvent.JBool(false)
						case JsonToken.VALUE_TRUE => JsonEvent.JBool(true)
						case JsonToken.VALUE_NUMBER_INT => JsonEvent.JLong(parser.getLongValue)
						case JsonToken.VALUE_NUMBER_FLOAT => JsonEvent.JDouble(parser.getDoubleValue)
						case JsonToken.VALUE_STRING => JsonEvent.JString(parser.getText)
						case JsonToken.VALUE_NULL => JsonEvent.JNull
						case t => JsonEvent.Unknown
					}

					// infer any extra context start/end events, injecting them into the returned buffer
					buffer = contextStack.headOption match {
						// If an array just started, any event but ArrayEnd should push an Index(0) context.
						case Some(JsonEvent.ArrayStart) if event != JsonEvent.ArrayEnd =>
							JsonEvent.IndexStart(0) :: event :: Nil

						// If we're in the middle of an array, any event but ArrayEnd should advance the index.
						case Some(JsonEvent.IndexStart(i)) if event != JsonEvent.ArrayEnd =>
							JsonEvent.IndexEnd :: JsonEvent.IndexStart(i + 1) :: event :: Nil

						// If we're inside an array index when the array ends, inject an IndexEnd first.
						case Some(JsonEvent.IndexStart(_)) if event == JsonEvent.ArrayEnd =>
							JsonEvent.IndexEnd :: event :: Nil

						// If we encounter a new field while already in a field, inject a FieldEnd first
						case Some(JsonEvent.FieldStart(_)) if event.isInstanceOf[JsonEvent.FieldStart] =>
							JsonEvent.FieldEnd :: event :: Nil

						// If the object ends while we're in a field, inject a FieldEnd first
						case Some(JsonEvent.FieldStart(_)) if event == JsonEvent.ObjectEnd =>
							JsonEvent.FieldEnd :: event :: Nil

						// All other events can be passed through normally
						case _ =>
							event :: Nil
					}

				}
			}

		}
	}
}