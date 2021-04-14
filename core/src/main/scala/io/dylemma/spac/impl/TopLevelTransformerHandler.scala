package io.dylemma.spac
package impl

import scala.util.control.NonFatal

class TopLevelTransformerHandler[In, Out](var current: Transformer.Handler[In, Out], caller: SpacTraceElement) extends Transformer.Handler[In, Out] {
	def step(in: In) = {
		try {
			val (emit, contOpt) = current.step(in)
			emit -> contOpt.map { cont =>
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
