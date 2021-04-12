package io.dylemma.spac
package impl

class ParserFirstOpt[In] extends Parser.Stateless[In, Option[In]] {
	def step(in: In) = Left(Some(in))
	def finish() = None
}
