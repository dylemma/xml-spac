package io.dylemma.spac.xml.handlers

import javax.xml.namespace.QName
import javax.xml.stream.events.XMLEvent

import io.dylemma.spac.Handler
import io.dylemma.spac.handlers.{FinishOnError, ManualFinish}

class OptionalAttributeHandler(name: QName)
	extends Handler[XMLEvent, Option[String]]
	with ManualFinish
	with FinishOnError
{

	override def toString = s"OptionalAttribute($name)"

	def handleEnd() = finishWith(None)

	def handleInput(input: XMLEvent): Option[Option[String]] = maybeFinishWith {
		if(input.isStartElement){
			val elem = input.asStartElement
			val attr = elem.getAttributeByName(name)
			Some{
				if(attr == null) None
				else Some(attr.getValue)
			}
		} else {
			None
		}
	}

}
