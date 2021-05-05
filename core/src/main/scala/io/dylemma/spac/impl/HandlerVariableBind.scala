package io.dylemma.spac
package impl

import scala.util.control.NonFatal

class HandlerVariableBind[-In, Out](handler: Transformer.Handler[In, Out]) extends Transformer.BoundHandler[In] with Transformer.HandlerLinkage[Out] {
	private var isStopped = false
	private var downstream: Transformer.HandlerWrite[Out] = Transformer.HandlerWrite.noopAndContinue

	def push(in: In): Signal = {
		if (isStopped) Signal.Stop
		else {
			val signal =
				try handler.push(in, downstream)
				catch {
					case NonFatal(e) =>
						isStopped = true
						handler.bubbleUp(e)
				}
			if (signal.isStop) isStopped = true
			signal
		}
	}

	def finish(): Unit = {
		if (!isStopped) {
			try handler.finish(downstream)
			catch {case NonFatal(e) => handler.bubbleUp(e)}
			finally isStopped = true
		}
	}

	def setDownstream(newDownstream: Transformer.HandlerWrite[Out]): Unit = {
		downstream = newDownstream
	}
}