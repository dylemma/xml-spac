package io.dylemma.xsp.handlers

import javax.xml.stream.events.XMLEvent

import scala.util.control.NoStackTrace

class XMLHandlerException(val msg: String, val event: Option[XMLEvent] = None)
	extends Exception(msg) with NoStackTrace

object XMLHandlerException {
	def apply(msg: String) = new XMLHandlerException(msg)
	def apply(msg: String, event: XMLEvent) = new XMLHandlerException(msg, Some(event))
}
