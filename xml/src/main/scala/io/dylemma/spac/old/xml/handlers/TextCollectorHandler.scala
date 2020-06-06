package io.dylemma.spac.old.xml.handlers

import javax.xml.stream.events.XMLEvent
import io.dylemma.spac.old.handlers.{FinishOnError, ManualFinish}
import io.dylemma.spac.old.Handler

class TextCollectorHandler
	extends Handler[XMLEvent, String]
	with ManualFinish
	with FinishOnError
{
	override def toString = "XMLText"
	private val sb = new StringBuilder
	def handleEnd(): String = finishWith(sb.result())
	def handleInput(input: XMLEvent) = {
		if(input.isCharacters) {
			sb append input.asCharacters.getData
		}
		None
	}
}
