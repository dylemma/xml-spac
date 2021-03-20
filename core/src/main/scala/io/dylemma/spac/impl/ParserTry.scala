package io.dylemma.spac
package impl

import scala.util.{Failure, Success, Try}

class ParserTry[In, Out](self: Parser[In, Out]) extends Parser[In, Try[Out]] {
	def newHandler = new ParserTry.Handler(self.newHandler)
}

object ParserTry {
	class Handler[In, Out](private var inner: Parser.Handler[In, Out]) extends Parser.Handler[In, Try[Out]] {
		def step(in: In) = {
			Try(inner step in) match {
				case Success(Left(out)) => Left(Success(out))
				case Success(Right(cont)) =>
					inner = cont
					Right(this)
				case Failure(e) => Left(Failure(e))
			}
		}
		def finish(): Try[Out] = Try { inner.finish() }
	}
}