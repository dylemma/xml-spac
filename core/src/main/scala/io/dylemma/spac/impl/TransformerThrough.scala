package io.dylemma.spac
package impl

class TransformerThrough[In, X, Out](first: Transformer[In, X], second: Transformer[X, Out]) extends Transformer[In, Out] {
	def newHandler = new TransformerThrough.Handler(first.newHandler, second.newHandler)
}

object TransformerThrough {
	class Handler[In, X, Out](
		private var first: Transformer.Handler[In, X],
		private var second: Transformer.Handler[X, Out]
	) extends Transformer.Handler[In, Out]
	{
		def step(in: In) = first.step(in) match {
			case (xEmit, None) =>
				// first is finished, so feed its results to the `second` parser and then finish that one too
				second.stepMany(xEmit) match {
					case (outs, Left(leftoverX)) => outs -> None
					case (outs, Right(continueSecond)) => (outs ++ continueSecond.finish()) -> None
				}

			case (xEmit, Some(continueFirst)) =>
				// first transformer emitted some values that need to be forwarded to the second, before continuing
				second.stepMany(xEmit) match {
					case (outs, Left(leftovers)) =>
						// second transformer ended as a result of receiving values from the first
						outs -> None

					case (outs, Right(continueSecond)) =>
						// both transformers are still running, update the state and continue
						first = continueFirst
						second = continueSecond
						outs -> Some(this)
				}
		}
		def finish() = {
			val xEmit = first.finish()
			// forward the results of finishing the first parser along to the second parser
			second.stepMany(xEmit) match {
				case (outs, Left(leftoverX)) =>
					// second parser finished as a result of the `xEmit` items, just return whatever it spit out as a result
					outs

				case (outs, Right(continueSecond)) =>
					// second parser is still active and needs to be finished; prepend the `outs` from the previous step to the final outputs
					outs ++ continueSecond.finish()
			}
		}
	}
}