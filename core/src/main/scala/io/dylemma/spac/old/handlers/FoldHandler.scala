package io.dylemma.spac.old.handlers

import io.dylemma.spac.old.Handler

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