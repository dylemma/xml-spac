package io.dylemma.spac.handlers

trait ManualFinish {
	private var _finished = false
	def isFinished = _finished

	protected def finishWith[T](value: => T) = {
		try { value }
		finally { _finished = true }
	}

	protected def maybeFinishWith[T](valueOpt: => Option[T]) = {
		try {
			val v = valueOpt
			if(v.isDefined) _finished = true
			v
		} catch {
			case e: Throwable =>
				_finished = true
				throw e
		}
	}
}
