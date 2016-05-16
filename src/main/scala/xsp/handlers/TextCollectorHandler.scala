package xsp.handlers

import javax.xml.stream.events.XMLEvent

import xsp.{Handler, Result}

class TextCollectorHandler
	extends Handler[XMLEvent, Result[String]]
	with ManualFinish
	with FinishOnError
{
	private val sb = new StringBuilder
	def handleEnd(): Result[String] = finishWith(Result(sb.result()))
	def handleInput(input: XMLEvent) = {
		if(input.isCharacters) {
			sb append input.asCharacters.getData
		}
		None
	}
}
