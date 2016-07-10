package io.dylemma.spac.handlers

import javax.xml.namespace.QName
import javax.xml.stream.events.XMLEvent

import io.dylemma.spac.{Handler, Result}

import scala.util.{Success, Try}

class OptionalAttributeHandler(name: QName)
	extends Handler[XMLEvent, Try[Option[String]]]
	with ManualFinish
	with FinishOnError
{

	override def toString = s"OptionalAttribute($name)"

	def handleEnd() = finishWith(Success(None))

	def handleInput(input: XMLEvent): Option[Try[Option[String]]] = maybeFinishWith {
		if(input.isStartElement){
			val elem = input.asStartElement
			val attr = elem.getAttributeByName(name)
			Some{
				if(attr == null) Success(None)
				else Success(Some(attr.getValue))
			}
		} else {
			None
		}
	}

}
