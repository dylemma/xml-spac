package io.dylemma.spac
package impl

class ParserDelay[A](result: () => A) extends Parser.Stateless[Any, A] {
	def step(in: Any) = Left(result())
	def finish() = result()
}
