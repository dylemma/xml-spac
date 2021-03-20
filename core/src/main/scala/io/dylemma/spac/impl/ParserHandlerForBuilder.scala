package io.dylemma.spac
package impl

import scala.collection.mutable

class ParserHandlerForBuilder[In, Out](builder: mutable.Builder[In, Out]) extends Parser.Handler[In, Out] {
	def step(in: In) = {
		builder += in
		Right(this)
	}
	def finish() = builder.result()
}
