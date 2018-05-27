package io.dylemma.spac.handlers

import io.dylemma.spac.Handler

/** Wrapper for a `downstream` handler which will enforce an expectation of
  * a specific sequence of events (as expressed by `expectations`).
  * If an input comes in that doesn't meet the expectations, or an EOF is received before
  * all of the expected inputs are encountered, this wrapper will send an error downstream.
  *
  * @param expectations A list containing a series of "label -> test" values representing the input expectations
  * @param downstream The wrapped handler
  * @tparam In  The input type
  * @tparam Out The output type
  */
class ExpectationSequenceHandler[In, Out](
	expectations: List[(String, In => Boolean)],
	downstream: Handler[In, Out]
) extends Handler[In, Out] {

	protected var remainingExpectations = expectations

	def isFinished = downstream.isFinished
	def handleError(error: Throwable) = downstream.handleError(error)

	def handleInput(input: In) = remainingExpectations match {
		case (label, test) :: tail =>
			if(test(input)){
				remainingExpectations = tail
				downstream.handleInput(input)
			} else {
				downstream.handleError {
					new IllegalArgumentException(s"Unexpected argument sent to [$downstream]: expected $label but got $input")
				}
			}
		case Nil =>
			downstream.handleInput(input)
	}

	def handleEnd() = remainingExpectations match {
		case Nil => downstream.handleEnd()
		case rem =>
			val labels = rem.map(_._1).mkString(", ")
			downstream.handleError {
				new IllegalStateException(s"Unexpected EOF sent to [$downstream]: still expected the following event(s): [$labels]")
			} getOrElse {
				downstream.handleEnd()
			}

	}
}
