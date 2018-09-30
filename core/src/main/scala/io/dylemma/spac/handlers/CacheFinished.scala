package io.dylemma.spac.handlers

/** Mixin for handlers with relatively-complex `isFinished` methods.
  * This trait moves the hard work into the `checkIsFinished` function,
  * guarding calls to it with a flag; once checkIsFinished returns true,
  * the flag is set to true so that isFinished can immediately return
  * true from then on.
  */
trait CacheFinished {
	protected def checkIsFinished: Boolean

	private var _isFinished = false

	def isFinished = {
		if(_isFinished) true
		else {
			_isFinished = checkIsFinished
			_isFinished
		}
	}
}
