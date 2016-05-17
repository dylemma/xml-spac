package xsp

import xsp.handlers._

/** An immutable object that can be used to create `Handler`s.
	*/
trait Consumer[-In, +Out] {
	self =>
	def makeHandler(): Handler[In, Out]
}

object Consumer {

	case class ToList[A]() extends Consumer[A, Result[List[A]]] {
		def makeHandler(): Handler[A, Result[List[A]]] = {
			new ToListHandler[A]
		}
	}
}