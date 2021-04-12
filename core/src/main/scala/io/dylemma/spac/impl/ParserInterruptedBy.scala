package io.dylemma.spac
package impl

class ParserInterruptedBy[In, Out](main: Parser[In, Out], interrupter: Parser[In, Any]) extends Parser[In, Out] {
	def newHandler = new ParserInterruptedBy.Handler(main.newHandler, interrupter.newHandler)
}

object ParserInterruptedBy {
	// note: if `interrupter` throws, then this parser will throw. If you want to treat errors in `interrupter` as an interruption, pass `interrupter.attempt` instead
	class Handler[In, Out](
		private var main: Parser.Handler[In, Out],
		private var interrupter: Parser.Handler[In, Any]
	) extends Parser.Handler[In, Out]
	{

		def step(in: In) = {
			interrupter.step(in) match {
				case Right(interrupterCont) =>
					interrupter = interrupterCont
					// no interruption, carry on as normal
					main.step(in) map { mainCont =>
						main = mainCont
						this
					}
				case Left(_) =>
					// the `interrupter` parser finished with a result. Pretend this is an EOF for the `main` parser
					Left(main.finish())
			}
		}
		def finish() = {
			// in case `interrupter` has some kind of side-effects, we'll run its `finish `effect now,
			// but we don't care about its result since we're finishing the `main` parser regardless
			interrupter.finish()
			main.finish()
		}
	}
}