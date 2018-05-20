package io.dylemma.spac.handlers

import io.dylemma.spac.Handler
import io.dylemma.spac.types.Stackable

import scala.collection.mutable.ArrayBuffer

/** Handler that forwards inputs to a first handler until that handler yields a result, at which point the result
  * is used to create a second handler, to which all further inputs are forwarded until it yields a result.
  *
  * Before the second handler is generated, the input "stack" is tracked so that it can be replayed to the second
  * handler (so that it can acquire the correct context even though it was not available when the inputs forming
  * that context were initially handled). The "stack" behavior is generalized with the `Stackable` typeclass.
  *
  * An example usage of this is to faciliate creation of a Parser for an XML element which starts with some "dictionary"
  * element, followed by a large number of "data" objects that reference the dictionary. Normally, a user of xml-spac
  * would have to choose between;
  *  - loading the large list of data objects into a List (high memory usage) in order to use regular parser combination methods like `and`
  *  - creating an external map and having the dictionary parser side effect by populating the map, and the data object parser read from
  *    that map (this is against the spirit of xml-spac since it breaks the immutability/encapsulation of the parser).
  * Instead, with this handler, the user can write a parser for the dictionary element and have its result be passed
  * into the parser for its sibling elements (the data elements). No external mutable state is required.
  *
  * @param handler1 The first handler
  * @param getHandler2 A function to generate the second handler from the first handler's result
  * @tparam In The input type
  * @tparam T1 The first handler's result type
  * @tparam T2 The second handler's result type
  */
class SequencedInStackHandler[In: Stackable, T1, T2](handler1: Handler[In, T1], getHandler2: T1 => Handler[In, T2]) extends Handler[In, T2] {
	private val stackable = implicitly[Stackable[In]]
	private val stack = new ArrayBuffer[stackable.StackElem]
	private var handler2Opt: Option[Handler[In, T2]] = None

	override def toString = {
		val h2String = handler2Opt.fold(getHandler2.toString)(_.toString)
		s"SequencedInStackHandler($handler1 :: $h2String)"
	}

	def isFinished = handler2Opt.fold(false)(_.isFinished)

	def handleInput(event: In): Option[T2] = {
		handler2Opt match {
			case None =>
				updateStack(event)
				// feed the event to handler1 to see if it gives a result
				handler1.handleInput(event) flatMap { initHandler2(_)._2 }
			case Some(handler2) =>
				handler2.handleInput(event)
		}
	}
	def handleError(error: Throwable): Option[T2] = handler2Opt match {
		case None =>
			// if handler1 doesn't rethrow or give a result, our only option is to rethrow.
			// otherwise, init handler2 from the result and forward the error to it
			handler1.handleError(error) match {
				case None => throw error
				case Some(h1Result) =>
					val (handler2, h2Result) = initHandler2(h1Result)
					h2Result orElse handler2.handleError(error)
			}
		case Some(handler2) =>
			handler2.handleError(error)
	}

	def handleEnd(): T2 = handler2Opt match {
		case None =>
			val h1Result = handler1.handleEnd()
			val (handler2, h2Result) = initHandler2(h1Result)
			h2Result getOrElse handler2.handleEnd()
		case Some(handler2) =>
			handler2.handleEnd()
	}

	/** Update the stack by pushing a StartElement event or popping
	  * in response to an EndElement.
	  */
	private def updateStack(event: In): Unit = {
		stackable.asPush(event) match {
			case Some(elem) => stack += elem
			case None if(stackable.isPop(event)) => stack.remove(stack.length - 1)
			case _ => ()
		}
	}

	/** Pass the StartElement events in the stack into `handler2` until either
	  * the stack is depleted or `handler2` returns a result.
	  */
	private def replayStack(handler2: Handler[In, T2]): Option[T2] = {
		def recurse(index: Int): Option[T2] = {
			if(index >= stack.length) None
			else handler2.handleInput(stack(index)) orElse { recurse(index+1) }
		}
		recurse(0)
	}

	/** Initializes the `handler2Opt` by creating handler2,
	  * then replays the current stack on handler2, returning
	  * the result of that replay.
	  */
	private def initHandler2(h1Result: T1): (Handler[In, T2], Option[T2]) = {
		val handler2 = getHandler2(h1Result)
		handler2Opt = Some(handler2)
		handler2 -> replayStack(handler2)
	}
}