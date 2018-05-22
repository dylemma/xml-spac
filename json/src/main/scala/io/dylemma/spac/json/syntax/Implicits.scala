package io.dylemma.spac.json.syntax

import io.dylemma.spac.json.{JsonEvent, JsonEvents, JsonParser, JsonResource, JsonSplitter, JsonStackElem}
import io.dylemma.spac.{ConsumableLike, FromHandlerFactory, SplitterApply}

trait Implicits {

	implicit val consumableLikeJsonEvents: ConsumableLike[JsonEvents, JsonEvent] = JsonEvents.consumableLike
	implicit def consumableLikeJsonResource[T: JsonResource]: ConsumableLike[T, JsonEvent] = JsonResource.consumableLike[T]

	implicit val jsonSplitterApply: SplitterApply[JsonStackElem, JsonSplitter] = JsonSplitter

	implicit val jsonParserFromHandlerFactory: FromHandlerFactory[JsonEvent, JsonParser] = JsonParser.handlerFactoryConverter
}
