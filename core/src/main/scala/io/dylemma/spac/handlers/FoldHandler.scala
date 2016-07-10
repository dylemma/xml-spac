package io.dylemma.spac.handlers

import io.dylemma.spac.Handler

class FoldHandler[A, R](init: R, f: (R, A) => R) extends Handler[A, R] with ManualFinish {
	override def toString = s"Fold($init, $f)"

	private var state = init

	def handleInput(input: A): Option[R] = {
		state = f(state, input)
		None
	}
	def handleError(err: Throwable) = finishWith { throw err }
	def handleEnd(): R = finishWith{ state }
}