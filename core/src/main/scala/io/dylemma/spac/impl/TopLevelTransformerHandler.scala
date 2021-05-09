package io.dylemma.spac
package impl

import scala.util.control.NonFatal

class TopLevelTransformerHandler[In, Out](inner: Transformer.Handler[In, Out], caller: SpacTraceElement) extends Transformer.Handler[In, Out] {
	def push(in: In, out: Transformer.HandlerWrite[Out]) = {
		try inner.push(in, out)
		catch {
			case NonFatal(e) => throw SpacException.addTrace(SpacException.addEarlyTrace(e, SpacTraceElement.InInput(in)), caller)
		}
	}
	def finish(out: Transformer.HandlerWrite[Out]) = {
		try inner.finish(out)
		catch { case NonFatal(e) => throw SpacException.addTrace(SpacException.addEarlyTrace(e, SpacTraceElement.AtInputEnd), caller) }
	}
}
