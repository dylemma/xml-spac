package io.dylemma.spac
package impl

import scala.util.control.NonFatal

class TransformerIntoParser[In, X, Out](t: Transformer[In, X], p: Parser[X, Out]) extends Parser[In, Out] {
	def newHandler = new TransformerIntoParser.Handler(t.newHandler, p.newHandler)
	override def toString = s"$t into $p"
}

object TransformerIntoParser {
	class Handler[In, X, Out](
		private var transformer: Transformer.Handler[In, X],
		private var parser: Parser.Handler[X, Out],
	) extends Parser.Handler[In, Out]
	{
		override def toString = s"TransformerIntoParser.Handler($transformer, $parser)"

		def step(in: In) = transformer.step(in) match {
			case (xEmit, Some(nextTransformer)) =>
				// transformer is emitting some values before continuing with a new state
				val stepResult =
					try { parser.stepMany(xEmit) }
					catch { case NonFatal(e) => throw nextTransformer.unwind(e) }
				stepResult match {
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
				val stepResult =
					try { parser.stepMany(xEmit) }
					catch { case NonFatal(e) => throw transformer.unwind(e) }
				stepResult match {
					// parser finished on its own
					case Left((out, leftovers)) => Left(out)
					// parser needs to be explicitly finished
					case Right(finalParser) => Left(finalParser.finish())
				}
		}

		def finish() = {
			val finalEmit = transformer.finish()
			try {
				parser.stepMany(finalEmit) match {
					case Left((out, leftovers)) => out
					case Right(finalParser) => finalParser.finish()
				}
			} catch {
				case NonFatal(e) => throw transformer.unwind(e)
			}
		}

	}
}