package io.dylemma.spac
package impl

import scala.util.control.NonFatal

class HandlerProtect[In, Out](inner: Transformer.Handler[In, Out]) extends Transformer.Handler[In, Out] {
	private var isStopped = false

	def push(in: In, out: Transformer.HandlerWrite[Out]): Signal = {
		if (isStopped) Signal.Stop
		else {
			val signal =
				try inner.push(in, out)
				catch {
					case NonFatal(e) =>
						isStopped = true
						inner.bubbleUp(e)
				}
			if (signal.isStop) isStopped = true
			signal
		}
	}

	def finish(out: Transformer.HandlerWrite[Out]): Unit = {
		if (!isStopped) {
			try inner.finish(out)
			catch {case NonFatal(e) => inner.bubbleUp(e)}
			finally isStopped = true
		}
	}

	override def bubbleUp(err: Throwable) = inner.bubbleUp(err)
}
