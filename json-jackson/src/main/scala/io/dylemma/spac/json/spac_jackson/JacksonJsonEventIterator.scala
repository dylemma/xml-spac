package io.dylemma.spac
package json
package spac_jackson

import com.fasterxml.jackson.core.{JsonToken, JsonParser => JacksonParser}

class JacksonJsonEventIterator(parser: JacksonParser) {

	private var contextStack: List[JsonStackElem] = Nil
	private var buffer: List[JsonEvent] = Nil
	private var isEOF = false

	private def currentLocation: ContextLocation = {
		val loc = parser.getCurrentLocation
		ContextLocation(
			ContextLineNumber ->> loc.getLineNr.longValue,
			ContextColumnOffset ->> loc.getColumnNr.longValue,
			ContextCharOffset ->> loc.getCharOffset
		)
	}

	def hasNext = {
		if (buffer.isEmpty && !isEOF) advance()
		buffer.nonEmpty
	}

	def next() = {
		if (buffer.isEmpty && !isEOF) advance()
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
		if (token == null) {
			// update the state to signify EOF
			buffer = Nil
			isEOF = true
		} else {
			isEOF = false

			val loc = currentLocation

			// create an event based on the parser's current "token"
			val tokenInterpretation: Option[JsonEvent] = token match {
				case JsonToken.START_OBJECT => Some(JsonEvent.ObjectStart(loc))
				case JsonToken.FIELD_NAME => Some(JsonEvent.FieldStart(parser.getCurrentName, loc))
				case JsonToken.END_OBJECT => Some(JsonEvent.ObjectEnd(loc))
				case JsonToken.START_ARRAY => Some(JsonEvent.ArrayStart(loc))
				case JsonToken.END_ARRAY => Some(JsonEvent.ArrayEnd(loc))
				case JsonToken.VALUE_FALSE => Some(JsonEvent.JBool(false, loc))
				case JsonToken.VALUE_TRUE => Some(JsonEvent.JBool(true, loc))
				case JsonToken.VALUE_NUMBER_INT => Some(JsonEvent.JLong(parser.getLongValue, loc))
				case JsonToken.VALUE_NUMBER_FLOAT => Some(JsonEvent.JDouble(parser.getDoubleValue, loc))
				case JsonToken.VALUE_STRING => Some(JsonEvent.JString(parser.getText, loc))
				case JsonToken.VALUE_NULL => Some(JsonEvent.JNull(loc))
				case _ => None
			}

			tokenInterpretation match {
				case Some(event) =>
					// infer any extra context start/end events, injecting them into the returned buffer
					buffer = contextStack.headOption match {
						// If an array just started, any event but ArrayEnd should push an Index(0) context.
						case Some(JsonEvent.ArrayStart()) if !event.isArrayEnd =>
							JsonEvent.IndexStart(0, loc) :: event :: Nil

						// If we're in the middle of an array, any event but ArrayEnd should advance the index.
						case Some(JsonEvent.IndexStart(e)) if !event.isArrayEnd =>
							JsonEvent.IndexEnd(loc) :: JsonEvent.IndexStart(e.index + 1, loc) :: event :: Nil

						// If we're inside an array index when the array ends, inject an IndexEnd first.
						case Some(JsonEvent.IndexStart(_)) if event.isArrayEnd =>
							JsonEvent.IndexEnd(loc) :: event :: Nil

						// If we encounter a new field while already in a field, inject a FieldEnd first
						case Some(JsonEvent.FieldStart(_)) if event.asFieldStart.isDefined =>
							JsonEvent.FieldEnd(loc) :: event :: Nil

						// If the object ends while we're in a field, inject a FieldEnd first
						case Some(JsonEvent.FieldStart(_)) if event.isObjectEnd =>
							JsonEvent.FieldEnd(loc) :: event :: Nil

						// All other events can be passed through normally
						case _ =>
							event :: Nil
					}

				case None =>
					// just advance past any token types we don't understand
					advance()
			}

		}
	}
}
