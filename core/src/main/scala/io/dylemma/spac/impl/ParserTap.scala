package io.dylemma.spac
package impl

class ParserTap[In](f: In => Unit) extends Parser.Stateless[In, Unit] {
	def step(in: In) = {
		f(in)
		Right(this)
	}
	def finish() = ()
}
