package io.dylemma.spac
package impl

class ParserFold[In, Out](init: Out, op: (Out, In) => Out) extends Parser[In, Out] {
	def newHandler = new ParserFold.Handler(init, op)
}
object ParserFold {
	class Handler[In, Out](private var state: Out, op: (Out, In) => Out) extends Parser.Handler[In, Out] {
		def step(in: In) = {
			state = op(state, in)
			Right(this)
		}
		def finish() = state
	}
}