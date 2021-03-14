package io.dylemma.spac
package impl

import cats.Monad
import cats.syntax.all._

// note: if `interrupter` throws, then this parser will throw. If you want to treat errors in `interrupter` as an interruption, pass `interrupter.attempt` instead
class ParserInterruptedBy[F[+_]: Monad, In, Out](main: Parser[F, In, Out], interrupter: Parser[F, In, Any]) extends Parser[F, In, Out] {
	def step(in: In): F[Either[Out, Parser[F, In, Out]]] = {
		interrupter.step(in).flatMap {
			case Right(interrupterCont) =>
				// no interruption, carry on as normal
				main.step(in).map { stepResult =>
					stepResult.map { mainCont =>
						new ParserInterruptedBy(mainCont, interrupterCont)
					}
				}
			case Left(_) =>
				// the `interrupter` parser finished with a result. Pretend this is an EOF for the `main` parser
				main.finish.map(Left(_))
		}
	}
	def finish: F[Out] = {
		// in case `interrupter` has some kind of side-effects, we'll run its `finish `effect now,
		// but we don't care about its result since we're finishing the `main` parser regardless
		interrupter.finish >> main.finish
	}
}
