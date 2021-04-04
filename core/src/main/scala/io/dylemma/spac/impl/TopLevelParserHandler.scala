package io.dylemma.spac
package impl

import scala.util.control.NonFatal

class TopLevelParserHandler[In, Out](var current: Parser.Handler[In, Out], caller: SpacTraceElement) extends Parser.Handler[In, Out] {
	def step(in: In) = {
		try {
			current.step(in) map { cont =>
				current = cont
				this
			}
		} catch {
			case NonFatal(e) => throw SpacException.addTrace(SpacException.addEarlyTrace(e, SpacTraceElement.InInput(in)), caller)
		}
	}
	def finish() = {
		try current.finish()
		catch { case NonFatal(e) => throw SpacException.addTrace(SpacException.addEarlyTrace(e, SpacTraceElement.AtInputEnd), caller) }
	}
}
