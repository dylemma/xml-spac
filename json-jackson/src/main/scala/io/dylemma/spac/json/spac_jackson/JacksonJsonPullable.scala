package io.dylemma.spac
package json
package spac_jackson

import cats.effect.Sync
import com.fasterxml.jackson.core.{JsonToken, JsonParser => JacksonParser}
import io.dylemma.spac.json.impl.JsonPopEventInjector

import scala.annotation.tailrec

object JacksonJsonPullable {
	def apply[F[+_]: Sync](parser: JacksonParser): Pullable[F, JsonEvent] = {
		new RawEventPullable[F](parser) through new JsonPopEventInjector
	}

	private class RawEventPullable[F[+_]: Sync](parser: JacksonParser) extends Pullable[F, JsonEvent] {
		private val F = Sync[F]
		def uncons = F.delay {
			// underlying `parser` behaves as an Iterator, so we can't be "pure", so the "tail"
			// that we return is just a reference to `this`, whose underlying `parser` has advanced
			next() map { _ -> this }
		}

		@tailrec private def next(): Option[JsonEvent] = {
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
					case _ => next()
				}
			}
		}

		private def currentLocation: ContextLocation = {
			val loc = parser.getCurrentLocation
			ContextLocation(
				ContextLineNumber ->> loc.getLineNr.longValue,
				ContextColumnOffset ->> loc.getColumnNr.longValue,
				ContextCharOffset ->> loc.getCharOffset
			)
		}
	}
}
