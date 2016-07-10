package io.dylemma.spac.handlers

import javax.xml.namespace.QName
import javax.xml.stream.events.XMLEvent

import io.dylemma.spac.{Handler, Result}

import scala.util.{Failure, Success, Try}

class MandatoryAttributeHandler(name: QName)
	extends Handler[XMLEvent, Try[String]]
	with ManualFinish
	with FinishOnError
{
	override def toString = s"Attribute($name)"

	@inline private def errorResult(msg: String, event: XMLEvent) = Failure(XMLHandlerException(msg, event))
	@inline private def errorResult(msg: String) = Failure(XMLHandlerException(msg))

	def handleEnd(): Try[String] = finishWith {
		errorResult("end reached before attribute was found")
	}

	def handleInput(input: XMLEvent) = maybeFinishWith {
		if (input.isStartElement) {
			val elem = input.asStartElement
			val attr = elem.getAttributeByName(name)
			Some {
				if (attr == null) errorResult(s"mandatory [$name] attribute missing", input)
				else Success(attr.getValue)
			}
		} else {
			None
		}
	}

}
