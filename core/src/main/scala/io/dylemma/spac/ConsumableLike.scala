package io.dylemma.spac

import scala.util.control.NonFatal

/** Adapter typeclass for push-style processingof a stream-like resource.
  *
  * @see [[Handler]] for pull-style processing
  * @tparam Resource
  * @tparam Item
  */
trait ConsumableLike[-Resource, +Item] {
	/** Given a resource and a handler, treat the resource as a stream,
	  * pushing items from the stream into the `handler` until either
	  * the handler returns a result or an EOF is reached.
	  *
	  * @param resource A resource
	  * @param handler A handler for items from the resource's stream
	  * @tparam A The result type
	  * @return A result returned by the `handler`
	  */
	def apply[A](resource: Resource, handler: Handler[Item, A]): A

	/** Helper method for implementing custom `ConsumableLike` instances.
	  * This will push items from the iterator into the handler until
	  * either the iterator runs out of items, or the handler is finished.
	  * If the iterator finishes before the handler, it will tell the
	  * handler to handle an "End" event in order to obtain a final result.
	  */
	protected def runIterator[In, Out](iterator: Iterator[In], handler: Handler[In, Out]): Out = {
		// try to be smart about closeable iterators
		val closeFunc = iterator match {
			case c: AutoCloseable => Some { () => try c.close() catch {case NonFatal(_) => ()} }
			case _ => None
		}
		var result: Option[Out] = None
		try {
			while (iterator.hasNext && !handler.isFinished) {
				result = handler.handleInput(iterator.next)
			}
			result getOrElse handler.handleEnd()
		} finally {
			// call the close function, if any
			closeFunc foreach {_ ()}
		}
	}
}

object ConsumableLike {

	// anything Iterable
	implicit def getIterableConsumable[T]: ConsumableLike[Iterable[T], T] = {
		new ConsumableLike[Iterable[T], T] {
			def apply[R](source: Iterable[T], handler: Handler[T, R]): R = {
				runIterator(source.iterator, handler)
			}
		}
	}

	// any Iterator
	implicit def getIteratorConsumable[T]: ConsumableLike[Iterator[T], T] = {
		new ConsumableLike[Iterator[T], T] {
			def apply[R](source: Iterator[T], handler: Handler[T, R]): R = {
				runIterator(source, handler)
			}
		}
	}
}
