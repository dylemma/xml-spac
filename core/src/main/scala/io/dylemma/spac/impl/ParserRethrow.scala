package io.dylemma.spac
package impl

import scala.util.Try

class ParserRethrow[In, Out](self: Parser[In, Try[Out]]) extends Parser[In, Out] {
	def newHandler = new ParserRethrow.Handler(self.newHandler)
}

object ParserRethrow {
	class Handler[In, Out](private var p: Parser.Handler[In, Try[Out]]) extends Parser.Handler[In, Out] {
		def step(in: In) = p.step(in) match {
			case Right(cont) =>
				p = cont
				Right(this)
			case Left(tryOut) => Left(tryOut.get)
		}

		def finish() = p.finish().get
	}
}
