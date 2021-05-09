package io.dylemma.spac
package impl

import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.collection.immutable.VectorBuilder

class IteratorTransform[In, Out](itr: Iterator[In], transformer: Transformer[In, Out], callerFrame: SpacTraceElement) extends AbstractIterator[Out] {
	private var emitItr: Iterator[Out] = Iterator.empty
	private val downstream = new Transformer.BoundHandler.ToBuilder(new VectorBuilder[Out])
	private val handler = Transformer.Handler.bindDownstream(transformer.newHandler.asTopLevelHandler(callerFrame), downstream)
	private var handlerFinished = false

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
	@tailrec private def stepUntilEmit(): Boolean = {
		if (handlerFinished) false
		else if (itr.hasNext) {
			// feed the next input to the handler and see if it outputs anything
			handlerFinished = handler.push(itr.next()).isStop
			emitItr = downstream.take().iterator
			if (emitItr.hasNext) true
			else stepUntilEmit()
		}
		else {
			// no more inputs, so feed an EOF to the handler
			handler.finish()
			emitItr = downstream.take().iterator
			emitItr.hasNext
		}
	}
}
