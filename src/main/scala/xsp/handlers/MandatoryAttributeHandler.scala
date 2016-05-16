package xsp.handlers

import javax.xml.namespace.QName
import javax.xml.stream.events.XMLEvent

import xsp.{Handler, Result}

class MandatoryAttributeHandler(name: QName)
	extends Handler[XMLEvent, Result[String]]
	with ManualFinish
	with FinishOnError
{
	@inline private def errorResult(msg: String, event: XMLEvent) = Result.Error(XMLHandlerException(msg, event))
	@inline private def errorResult(msg: String) = Result.Error(XMLHandlerException(msg))

	def handleEnd(): Result[String] = finishWith {
		errorResult("end reached before attribute was found")
	}

	def handleInput(input: XMLEvent) = maybeFinishWith {
		if (input.isStartElement) {
			val elem = input.asStartElement
			val attr = elem.getAttributeByName(name)
			Some {
				if (attr == null) errorResult(s"mandatory [$name] attribute missing", input)
				else Result.Success(attr.getValue)
			}
		} else {
			None
		}
	}

}
