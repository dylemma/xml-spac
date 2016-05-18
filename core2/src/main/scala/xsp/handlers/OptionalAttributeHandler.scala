package xsp.handlers

import javax.xml.namespace.QName
import javax.xml.stream.events.XMLEvent

import xsp.{Handler, Result}

class OptionalAttributeHandler(name: QName)
	extends Handler[XMLEvent, Result[Option[String]]]
	with ManualFinish
	with FinishOnError
{

	override def toString = s"OptionalAttribute($name)"

	def handleEnd() = finishWith(Result.Success.none)

	def handleInput(input: XMLEvent): Option[Result[Option[String]]] = maybeFinishWith {
		if(input.isStartElement){
			val elem = input.asStartElement
			val attr = elem.getAttributeByName(name)
			Some{
				if(attr == null) Result.Success.none
				else Result.Success(Some(attr.getValue))
			}
		} else {
			None
		}
	}

}
