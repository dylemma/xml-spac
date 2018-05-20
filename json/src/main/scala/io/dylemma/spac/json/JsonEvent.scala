package io.dylemma.spac.json

/** ADT for tokens in a JSON stream.
  */
sealed trait JsonEvent
object JsonEvent {

	case object ObjectStart extends JsonEvent
	case class ObjectField(name: String) extends JsonEvent
	case object ObjectEnd extends JsonEvent

	case object ArrayStart extends JsonEvent
	case object ArrayEnd extends JsonEvent

	/** Base trait for JSONEvents representing primitive values */
	sealed abstract class JsonValueEvent(_toString: => String) extends JsonEvent {
		def valueAsString = _toString
	}

	case class JBool(bool: Boolean) extends JsonValueEvent(bool.toString)
	case class JLong(num: Long) extends JsonValueEvent(num.toString)
	case class JDouble(num: Double) extends JsonValueEvent(num.toString)
	case class JString(string: String) extends JsonValueEvent(string)
	case object JNull extends JsonValueEvent("null")

	case object Unknown extends JsonEvent
}
