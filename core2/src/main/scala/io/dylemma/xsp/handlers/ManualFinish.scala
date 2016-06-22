package io.dylemma.xsp.handlers

trait ManualFinish {
	private var _finished = false
	def isFinished = _finished
	protected def finishWith[T](value: T) = {
		_finished = true
		value
	}
	protected def maybeFinishWith[T](valueOpt: Option[T]) = {
		if(valueOpt.isDefined) _finished = true
		valueOpt
	}
}
