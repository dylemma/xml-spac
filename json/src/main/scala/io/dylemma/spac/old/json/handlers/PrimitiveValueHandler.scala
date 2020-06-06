package io.dylemma.spac.old.json.handlers

import io.dylemma.spac.old.Handler
import io.dylemma.spac.old.handlers.ManualFinish
import io.dylemma.spac.old.json.{JsonEvent, JsonValueEvent}

class PrimitiveValueHandler[A](typeName: String, f: JsonValueEvent => Option[A]) extends Handler[JsonEvent, A] with ManualFinish {
	def this(typeName: String, pf: PartialFunction[JsonValueEvent, A]) = this(typeName, pf.lift)

	override def toString = s"PrimitiveValueHandler($typeName)"

	def handleInput(input: JsonEvent) = finishWith {
		val extracted = input match {
			case ve: JsonValueEvent => f(ve)
			case _ => None
		}
		extracted orElse {
			throw new IllegalArgumentException(s"Illegal input to [$this]: Expected a $typeName but encountered $input")
		}
	}

	def handleError(error: Throwable) = finishWith {
		throw new Exception(s"Bubbling exception caught by [$this]", error)
	}

	def handleEnd() = finishWith {
		throw new IllegalArgumentException(s"Unexpected EOF or End of Context sent to [$this]: Expected a $typeName")
	}
}
