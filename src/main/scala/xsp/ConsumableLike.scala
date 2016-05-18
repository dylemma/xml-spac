package xsp

import javax.xml.stream.events.XMLEvent

import scala.util.control.NonFatal

trait ConsumableLike[-S, +E] {
	def apply[R](source: S, handler: Handler[E, R]): R

	/** Helper method for implementing custom `ConsumableLike` instances.
		* This will push items from the iterator into the handler until
		* either the iterator runs out of items, or the handler is finished.
		* If the iterator finishes before the handler, it will tell the
		* handler to handle an "End" event in order to obtain a final result.
		*/
	protected def runIterator[In, Out](iterator: Iterator[In], handler: Handler[In, Out]): Out = {
		// try to be smart about closeable iterators
		val closeFunc = iterator match {
			case c: AutoCloseable => Some{ () => try c.close() catch { case NonFatal(_) => () } }
			case _ => None
		}
		var result: Option[Out] = None
		try {
			while(iterator.hasNext && !handler.isFinished){
				result = handler.handleInput(iterator.next)
			}
			result getOrElse handler.handleEnd()
		} finally {
			// call the close function, if any
			closeFunc foreach { _() }
		}
	}
}

object ConsumableLike {
	// XMLEvents
	implicit object XMLEventsConsumableLike$ extends ConsumableLike[XMLEvents, XMLEvent] {
		def apply[R](source: XMLEvents, handler: Handler[XMLEvent, R]): R = {
			runIterator(source.iterator, handler)
		}
	}

	// anything in the XMLResource typeclass
	implicit def getXMLResourceConsumable[T: XMLResource]: ConsumableLike[T, XMLEvent] = {
		new ConsumableLike[T, XMLEvent] {
			def apply[R](source: T, handler: Handler[XMLEvent, R]): R = {
				runIterator(XMLEvents(source).iterator, handler)
			}
		}
	}

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

//	protected def guessCloseFunc(iterator: Iterator[_]): Function0[Unit] = iterator match {
//		case c: AutoCloseable => () => try c.close() catch { case NonFatal(_) => () }
//		case _ => () => ()
//	}
//
//	protected def runIterator[In, Out](
//		iterator: Iterator[In],
//		close: () => Unit,
//		handler: Handler[In, Out]
//	): Out = {
//		// try to be smart about closeable iterators
//		val closeFunc = iterator match {
//			case c: AutoCloseable => Some{ () => c.close() }
//			case _ => None
//		}
//		var result: Option[Out] = None
//		try {
//			while(iterator.hasNext && !handler.isFinished){
//				result = handler.handleInput(iterator.next)
//			}
//			result getOrElse handler.handleEnd()
//		} finally {
//			// call the close function, if any
//			close()
//		}
//	}
}
