package io.dylemma.spac

/** Value used by `Transformer.Handler` to indicate to its upstream producer
  * whether or not the handler wants to continue receiving values.
  */
sealed trait Signal {
	def isStop: Boolean

	/** Returns `Signal.Stop` if both `this` and `that` are the "stop" signal
	  *
	  * @param that
	  * @return
	  */
	def &&(that: Signal): Signal = Signal.stopIf(this.isStop && that.isStop)

	/** Returns `Signal.Stop` if at least one of `this` or `that` are the "stop" signal.
	  * Uses call-by-name semantics on `that`, to support short-circuiting evaluation in case `this` is known to be a `Stop`.
	  * E.g.
	  * {{{
	  *    out.push(1) || out.push(2)
	  * }}}
	  * Where if the `out.push(1)` call returns `Stop`, the `out.push(2)` expression will not be run.
	  *
	  * @param that
	  * @return
	  */
	def ||(that: => Signal): Signal = {
		if (this.isStop) Signal.Stop
		else if (that.isStop) Signal.Stop
		else Signal.Continue
	}
}
object Signal {
	def stopIf(shouldStop: Boolean): Signal = if (shouldStop) Stop else Continue
	def continueIf(shouldContinue: Boolean): Signal = if (shouldContinue) Continue else Stop

	case object Stop extends Signal {
		def isStop = true
	}
	case object Continue extends Signal {
		def isStop = false
	}
}