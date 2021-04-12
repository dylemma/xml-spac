package io.dylemma.spac
package impl

class ParserPure[Out](value: Out) extends Parser.Stateless[Any, Out] {
	def step(in: Any) = Left(value)
	def finish() = value
	override def toString = s"Parser.pure($value)"
}
