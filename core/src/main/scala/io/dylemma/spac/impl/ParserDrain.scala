package io.dylemma.spac
package impl

object ParserDrain extends Parser.Stateless[Any, Unit] {
	def step(in: Any) = Right(this)
	def finish() = ()
}
