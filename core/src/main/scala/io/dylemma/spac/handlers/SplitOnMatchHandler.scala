package io.dylemma.spac.handlers

import io.dylemma.spac.{Handler, Result, debug}

import scala.util.{Failure, Success, Try}

class SplitOnMatchHandler[In, Context, P, Out](
	matcher: PartialFunction[In, Context],
	makeInnerHandler: Context => Handler[In, Try[P]],
	val downstream: Handler[P, Out]
) extends SplitterHandlerBase[In, Context, P, Out]{

	protected def debugName = s"Splitter($matcher)"

	def handleInput(input: In): Option[Out] = {
		if(matcher isDefinedAt input){
			// end the current substream, feeding the inner result downstream
			val downstreamResult = feedEndToCurrentParser()
			  .map(debug as "Got inner parser result (from context end)")
			  .flatMap(feedResultToDownstream)

			// if the inner result did not cause the downstream to end, start a new substream
			downstreamResult.orElse {
				currentParserHandler = Try(matcher(input)) match {
					case Success(ctx) => Some(makeInnerHandler(ctx))
					case Failure(err) => Some(new OneShotHandler(Failure(err)))
				}

				// feed the input to the newly-created inner handler, possibly getting an input
				// to feed to the downstream handler
				feedEventToCurrentParser(input)
				  .map(debug as "Got inner parser result (from context begin input)")
				  .flatMap(feedResultToDownstream)
			}
		} else {
			// continue the current substream by passing this input to the inner handler
			feedEventToCurrentParser(input)
			  .map(debug as "Got inner parer result (from input)")
			  .flatMap(feedResultToDownstream)
		}
	}
}
