package io.dylemma.spac
package impl

class TransformerIntoParser[In, X, Out](t: Transformer[In, X], p: Parser[X, Out]) extends Parser[In, Out] {
	def newHandler = new TransformerIntoParser.Handler(t.newHandler, p.newHandler)
}

object TransformerIntoParser {
	class Handler[In, X, Out](
		private var transformer: Transformer.Handler[In, X],
		private var parser: Parser.Handler[X, Out]
	) extends Parser.Handler[In, Out]
	{

		def step(in: In) = transformer.step(in) match {
			case (xEmit, Some(nextTransformer)) =>
				// transformer is emitting some values before continuing with a new state
				parser.stepMany(xEmit) match {
					// parser finished
					case Left((out, leftovers)) => Left(out)
					// parser didn't finish, so we can continue from its new state
					case Right(nextParser) =>
						transformer = nextTransformer
						parser = nextParser
						Right(this)
				}

			case (xEmit, None) =>
				// transformer has finished, so after the `xEmit` that's the EOF so the parser may need to be finished
				parser.stepMany(xEmit) match {
					// parser finished on its own
					case Left((out, leftovers)) => Left(out)
					// parser needs to be explicitly finished
					case Right(finalParser) => Left(finalParser.finish())
				}
		}

		def finish() = {
			val finalEmit = transformer.finish()
			parser.stepMany(finalEmit) match {
				case Left((out, leftovers)) => out
				case Right(finalParser) => finalParser.finish()
			}
		}

	}
}