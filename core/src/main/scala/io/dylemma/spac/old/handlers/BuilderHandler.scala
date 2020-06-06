package io.dylemma.spac.old.handlers

import io.dylemma.spac.old.Handler

import scala.collection.mutable

class BuilderHandler[A, Coll](builder: mutable.Builder[A, Coll]) extends Handler[A, Coll] with ManualFinish with FinishOnError {
	def handleInput(input: A): Option[Coll] = {
		builder += input
		None
	}
	def handleEnd(): Coll = finishWith {
		builder.result()
	}
}
