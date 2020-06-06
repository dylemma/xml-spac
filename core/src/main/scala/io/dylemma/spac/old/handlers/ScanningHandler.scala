package io.dylemma.spac.old.handlers

import io.dylemma.spac.old.Handler

/** A handler that starts with some initial state, and each for each input the
	* current state is forwarded to the downstream handler, and the state is updated
	* according to the provided function `f`. The final state is passed when
	* `handleEnd` is called.
	*/
class ScanningHandler[S, A, Out](init: S, f: (S, A) => S, val downstream: Handler[S, Out]) extends TransformerHandler[A, S, Out] {
	override def toString = s"Scan($init, $f) >> $downstream"
	private var state = init
	protected def transformInput(input: A): Option[S] = {
		state = f(state, input)
		Some(state)
	}
}
