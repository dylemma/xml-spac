package io.dylemma.spac.json

import io.dylemma.spac.types.Stackable

/** ADT for tokens in a JSON stream.
  */
sealed trait JsonEvent

/** Subset of JsonEvents that constitute a "context stack push". */
sealed trait JsonStackElem extends JsonEvent

/** Subset of JsonEvents that constitute a "context stack pop". */
sealed trait JsonStackPop extends JsonEvent

/** Subset of JsonEvents that represent a primitive values */
sealed abstract class JsonValueEvent(_toString: => String) extends JsonEvent {
	def valueAsString = _toString
}

object JsonEvent {

	implicit val stackable: Stackable.Aux[JsonEvent, JsonStackElem] = new Stackable[JsonEvent] {
		type StackElem = JsonStackElem
		def isPush(elem: JsonEvent) = elem.isInstanceOf[JsonStackElem]
		def isPop(elem: JsonEvent) = elem.isInstanceOf[JsonStackPop]
		def asPush(elem: JsonEvent) = elem match {
			case push: JsonStackElem => Some(push)
			case _ => None
		}
		def order(event: JsonEvent) = event match {
			case ObjectStart | ArrayStart => 1 // include after the context switch
			case ObjectEnd | ArrayEnd => -1 // include in the context it's closing
			case IndexStart(_) | FieldStart(_) => -1 // exclude from the new context
			case IndexEnd | FieldEnd => 1 // exclude from the ending context
			case _ => 0 // normal events
		}
	}

	case object ObjectStart extends JsonStackElem
	case object ObjectEnd extends JsonStackPop

	case class FieldStart(name: String) extends JsonStackElem
	case object FieldEnd extends JsonStackPop

	case object ArrayStart extends JsonStackElem
	case object ArrayEnd extends JsonStackPop

	case class IndexStart(index: Int) extends JsonStackElem
	case object IndexEnd extends JsonStackPop


	case class JBool(bool: Boolean) extends JsonValueEvent(bool.toString)
	case class JLong(num: Long) extends JsonValueEvent(num.toString)
	case class JDouble(num: Double) extends JsonValueEvent(num.toString)
	case class JString(string: String) extends JsonValueEvent(string)
	case object JNull extends JsonValueEvent("null")

	case object Unknown extends JsonEvent
}
