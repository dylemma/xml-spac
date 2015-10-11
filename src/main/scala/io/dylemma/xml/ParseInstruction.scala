package io.dylemma.xml

import scala.language.implicitConversions

/** Used by the XMLEventSource's loop to decide whether to continue parsing
  * or stop.
  */
sealed trait ParseInstruction
case object ContinueParsing extends ParseInstruction
case object StopParsing extends ParseInstruction

object ParseInstruction {
	/** Helper for the XMLEventSource's `foreach` loop. Allows the event handler
	  * function to return any value to implicitly tell the parser to continue,
	  * but explicitly return a `StopParsing` to signal the end.
	  */
	implicit def any2ParseInstruction(a: Any): ParseInstruction = {
		if (a == StopParsing) StopParsing
		else ContinueParsing
	}
}
