package io.dylemma.spac

sealed trait Signal {
	def isStop: Boolean

	def &&(that: Signal): Signal = Signal.stopIf(this.isStop && that.isStop)
	def ||(that: => Signal): Signal = Signal.stopIf(this.isStop || that.isStop)
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