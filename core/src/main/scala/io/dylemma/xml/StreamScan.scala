package io.dylemma.xml

/** Stream transformation function that operates like a `foldLeft`.
	*
	* The scan is represented in terms of an `init` function that creates
	* a "blank" state, a `fold` function that updates a state based on an
	* input and returns results, and a `finish` function that generates a
	* final result from a leftover state when an EOF is encountered.
	*
	* @tparam A The input type
	* @tparam B The output type
	*/
trait StreamScan[A, B] {

	/** Represents the state of the stream at any given point.
		* It is recommended that the State should be immutable.
		*/
	type State

	/** Create an "empty" state */
	def init: State

	/** Advance the given `state` by adding an `input`.
		*
		* @param state The old state
		* @param input The incoming value
		* @return The new state, paired with a Result to emit for that step
		*/
	def fold(state: State, input: A): (State, Result[B])

	/** Finish the scan, creating a result from whatever leftover state is available.
		*
		* @param state The leftover state
		*/
	def finish(state: State): Result[B]
}