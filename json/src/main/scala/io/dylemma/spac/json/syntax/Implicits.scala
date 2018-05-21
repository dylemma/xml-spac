package io.dylemma.spac.json.syntax

import io.dylemma.spac.ConsumableLike
import io.dylemma.spac.handlers.ContextTracker
import io.dylemma.spac.json.{JsonContextTracker, JsonEvent, JsonEvents, JsonResource, JsonStackElem}
import io.dylemma.spac.types.Stackable

trait Implicits {

	implicit val jsonStackable: Stackable.Aux[JsonEvent, JsonStackElem] = new Stackable[JsonEvent] {
		type StackElem = JsonStackElem
		def isPush(event: JsonEvent) = { event == JsonEvent.ObjectStart || event == JsonEvent.ArrayStart }
		def asPush(event: JsonEvent) = event match {
			case JsonEvent.ObjectStart => Some(JsonStackElem.Object)
			case JsonEvent.ArrayStart => Some(JsonStackElem.Array)
			case _ => None
		}
		def isPop(event: JsonEvent) = { event == JsonEvent.ObjectEnd || event == JsonEvent.ArrayEnd }
	}

	implicit val consumableLikeJsonEvents: ConsumableLike[JsonEvents, JsonEvent] = JsonEvents.consumableLike
	implicit def consumableLikeJsonResource[T: JsonResource]: ConsumableLike[T, JsonEvent] = JsonResource.consumableLike[T]

	implicit def makeContextTracker: ContextTracker[JsonEvent, JsonStackElem] = new JsonContextTracker
}
