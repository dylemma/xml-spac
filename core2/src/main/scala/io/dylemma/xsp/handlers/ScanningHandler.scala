package io.dylemma.xsp.handlers

import io.dylemma.xsp.Handler

import scala.util.control.NonFatal

/** A handler that starts with some initial state, and each for each input the
	* current state is forwarded to the inner handler, and the state is updated
	* according to the provided function `f`. The final state is passed when
	* `handleEnd` is called.
	*/
class ScanningHandler[S, A, Out](init: S, f: (S, A) => S, inner: Handler[S, Out])
	extends Handler[A, Out]
	with ManualFinish
{
	override def toString = s"Scan($init, $f) >> $inner"

	private var state = init

	def handleInput(input: A): Option[Out] = {
		try {
			state = f(state, input)
			maybeFinishWith{ inner.handleInput(state) }
		} catch {
			case NonFatal(err) => maybeFinishWith { inner.handleError(err) }
		}
	}

	def handleError(error: Throwable): Option[Out] = maybeFinishWith { inner.handleError(error) }

	def handleEnd(): Out = {
		finishWith(inner.handleEnd())
	}
}
