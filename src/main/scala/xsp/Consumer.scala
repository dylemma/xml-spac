package xsp

/** An immutable object that can be used to create `Handler`s.
	*/
trait Consumer[-In, +Out] {
	def makeHandler(): Handler[In, Out]
}
