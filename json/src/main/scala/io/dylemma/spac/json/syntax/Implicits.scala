package io.dylemma.spac.json.syntax

import io.dylemma.spac.json.{JsonEvent, JsonEvents, JsonParser, JsonResource}
import io.dylemma.spac.{ConsumableLike, FromHandlerFactory}

trait Implicits {

	implicit val consumableLikeJsonEvents: ConsumableLike[JsonEvents, JsonEvent] = JsonEvents.consumableLike
	implicit def consumableLikeJsonResource[T: JsonResource]: ConsumableLike[T, JsonEvent] = JsonResource.consumableLike[T]

	implicit class JsonParserExtras[T](parser: JsonParser[T]) {
		def nullable: JsonParser[Option[T]] = JsonParser.nullable(parser)
	}

	implicit def jsonStringParser: JsonParser[String] = JsonParser.forString
	implicit def jsonIntParser: JsonParser[Int] = JsonParser.forInt
	implicit def jsonLongParser: JsonParser[Long] = JsonParser.forLong
	implicit def jsonFloatParser: JsonParser[Float] = JsonParser.forFloat
	implicit def jsonDoubleParser: JsonParser[Double] = JsonParser.forDouble
	implicit def jsonBooleanParser: JsonParser[Boolean] = JsonParser.forBoolean
	implicit def jsonNullParser: JsonParser[None.type] = JsonParser.forNull
}
