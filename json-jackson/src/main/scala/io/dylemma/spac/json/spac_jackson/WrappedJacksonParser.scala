package io.dylemma.spac
package json
package spac_jackson

import java.io.IOException

import com.fasterxml.jackson.core.{JsonToken, JsonParser => JacksonParser}

import scala.annotation.tailrec

private[spac_jackson] class WrappedJacksonParser(parser: JacksonParser) extends Iterator[JsonEvent] with AutoCloseable {
	def close() = parser.close()

	private var pendingNext: Option[JsonEvent] = None

	def hasNext = {
		if (pendingNext.isEmpty) pendingNext = maybeNext()
		pendingNext.isDefined
	}
	def next() = pendingNext match {
		case None =>
			maybeNext() getOrElse {
				throw new NoSuchElementException("called next() when hasNext was false")
			}
		case Some(e) =>
			pendingNext = None
			e
	}

	def maybeNext(): Option[JsonEvent] = {
		try _maybeNext()
		catch { case e: IOException =>
			latestLocation match {
				case None => throw e
				case Some(loc) => throw SpacException.addTrace(e, SpacTraceElement.NearLocation(loc))
			}
		}
	}

	@tailrec private def _maybeNext(): Option[JsonEvent] = {
		val token = parser.nextToken()
		if (token == null) {
			None
		} else {
			val loc = currentLocation
			token match {
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
				case _ => _maybeNext()
			}
		}
	}

	private var latestLocation: Option[ContextLocation] = None

	private def currentLocation: ContextLocation = {
		val loc = parser.getCurrentLocation
		val result = ContextLocation(
			ContextLineNumber ->> loc.getLineNr.longValue,
			ContextColumnOffset ->> loc.getColumnNr.longValue,
			ContextCharOffset ->> loc.getCharOffset
		)
		latestLocation = Some(result)
		result
	}


}
