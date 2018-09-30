package io.dylemma.spac.xml.handlers

import javax.xml.namespace.QName
import javax.xml.stream.events.XMLEvent

import io.dylemma.spac.Handler
import io.dylemma.spac.handlers.{FinishOnError, ManualFinish}

class MandatoryAttributeHandler(name: QName)
	extends Handler[XMLEvent, String]
	with ManualFinish
	with FinishOnError
{
	override def toString = s"Attribute($name)"

	@inline private def errorResult(msg: String, event: XMLEvent) = XMLHandlerException(msg, event)
	@inline private def errorResult(msg: String) = XMLHandlerException(msg)

	def handleEnd(): String = finishWith {
		throw errorResult("end reached before attribute was found")
	}

	def handleInput(input: XMLEvent) = maybeFinishWith {
		if (input.isStartElement) {
			val elem = input.asStartElement
			val attr = elem.getAttributeByName(name)
			Some {
				if (attr == null) throw errorResult(s"mandatory [$name] attribute missing", input)
				else attr.getValue
			}
		} else {
			None
		}
	}

}
