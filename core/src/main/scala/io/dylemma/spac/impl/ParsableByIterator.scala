package io.dylemma.spac
package impl

import scala.annotation.tailrec

abstract class ParsableByIterator[S, In] extends Parsable[cats.Id, S, In] {
	protected def lendIterator[Out](source: S, f: Iterator[In] => Out): Out

	def parse[Out](source: S, callerFrame: SpacTraceElement, parser: Parser[In, Out]) = {
		lendIterator(source, iterator => {
			@tailrec def loop(handler: Parser.Handler[In, Out]): Out = {
				if (iterator.hasNext) {
					val in = iterator.next()
					handler.step(in) match {
						case Left(out) => out
						case Right(cont) => loop(cont)
					}
				} else {
					handler.finish()
				}
			}

			loop(parser.newHandler.asTopLevelHandler(callerFrame))
		})
	}
}
