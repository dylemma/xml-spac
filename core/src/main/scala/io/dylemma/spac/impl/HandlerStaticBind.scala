package io.dylemma.spac
package impl

import scala.util.control.NonFatal

class HandlerStaticBind[-In, +Out](handler: Transformer.Handler[In, Out], downstream: Transformer.HandlerWrite[Out]) extends Transformer.BoundHandler[In] {
	private var isStopped = false

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


}
