package io.dylemma.spac.json

sealed trait JsonStackElem
object JsonStackElem {
	case object Object extends JsonStackElem
	case object Array extends JsonStackElem
}

