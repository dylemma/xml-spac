package io.dylemma.spac
package impl

import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.util.control.NonFatal

class IteratorTransform[In, Out](itr: Iterator[In], transformer: Transformer[In, Out]) extends AbstractIterator[Out] {
	private var emitItr: Iterator[Out] = Iterator.empty
	private var nextHandler: Option[Transformer.Handler[In, Out]] = Some(transformer.newHandler)

	def next() = {
		if (emitItr.hasNext) emitItr.next()
		else {
			stepUntilEmit()
			emitItr.next() // will throw if `stepUntilEmit` was false
		}
	}

	def hasNext = {
		if (emitItr.hasNext) true
		else stepUntilEmit()
	}

	// precondition: `emitItr.hasNext` is false
	// post-condition: either `emitItr.hasNext` is true or `nextHandler` is None
	// returns `emitItr.hasNext` when the post-condition is met
	@tailrec private def stepUntilEmit(): Boolean = nextHandler match {
		case Some(handler) =>
			// There is still a handler, which means we can try to feed inputs to it to get outputs
			if (itr.hasNext) {
				// feed the next input to the handler
				val in = itr.next()
				val (nextEmit, contHandler) =
					try handler.step(in)
					catch { case NonFatal(e) => throw SpacException.addEarlyTrace(e, SpacTraceElement.InInput(in)) }
				emitItr = nextEmit.iterator
				nextHandler = contHandler
				if (emitItr.hasNext) {
					true
				} else {
					stepUntilEmit() // this "emit" was empty, so continue the loop
				}
			} else {
				// no more inputs, so feed an EOF to the handler
				emitItr =
					try handler.finish().iterator
					catch { case NonFatal(e) => throw SpacException.addEarlyTrace(e, SpacTraceElement.AtInputEnd) }
				nextHandler = None
				emitItr.hasNext
			}

		case None =>
			// the transformer ended, so we don't care about the input `itr` anymore
			false
	}
}
