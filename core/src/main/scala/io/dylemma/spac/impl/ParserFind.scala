package io.dylemma.spac
package impl

class ParserFind[In](predicate: In => Boolean) extends Parser.Stateless[In, Option[In]] {
	def step(in: In) = {
		if (predicate(in)) Left(Some(in))
		else Right(this)
	}
	def finish() = None
}