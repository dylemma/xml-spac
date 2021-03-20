package io.dylemma.spac
package impl

class ParserExpectInputs[In, Out](p: Parser[In, Out], pendingExpectations: List[(String, In => Boolean)]) extends Parser[In, Out] {
	def newHandler = new ParserExpectInputs.Handler(p.newHandler, pendingExpectations)
}

object ParserExpectInputs {
	class Handler[In, Out](var self: Parser.Handler[In, Out], var pendingExpectations: List[(String, In => Boolean)]) extends Parser.Handler[In, Out] {
		def step(in: In) = pendingExpectations match {
			case Nil => self.step(in) // no longer wrapping the parser because all the expected inputs are passed
			case (expectation, predicate) :: tailExpectations =>
				if (predicate(in)) {
					// this input meets our expectation; carry on
					self.step(in) map { cont =>
						self = cont
						pendingExpectations = tailExpectations
						this
					}
				} else {
					// input failed our expectations, crash the parser with an error
					throw new SpacException.UnexpectedInputException(in, expectation :: tailExpectations.map(_._1))
				}
		}

		def finish() = {
			if (pendingExpectations.isEmpty) self.finish()
			else throw new SpacException.UnfulfilledInputsException(pendingExpectations.map(_._1))
		}
	}
}
