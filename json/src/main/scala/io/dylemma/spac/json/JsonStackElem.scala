package io.dylemma.spac.json

sealed trait JsonStackElem
object JsonStackElem {
	case object Object extends JsonStackElem
	case class Field(name: String) extends JsonStackElem
	case object Array extends JsonStackElem
	case class Index(index: Int) extends JsonStackElem
}

