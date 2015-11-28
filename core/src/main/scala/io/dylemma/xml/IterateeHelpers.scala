package io.dylemma.xml

import javax.xml.namespace.QName
import javax.xml.stream.events.XMLEvent
import scala.collection.generic.CanBuildFrom
import scala.concurrent.ExecutionContext
import scala.util.Try

import io.dylemma.xml.Result.{ Error, Empty, Success }
import io.dylemma.xml.event.{ EndElement, StartElement }
import play.api.libs.iteratee._

/**
 * Created by dylan on 10/10/2015.
 */
object IterateeHelpers extends IterateeHelpers
trait IterateeHelpers {

	/** Create an enumeratee that pairs its inputs with an accumulated state.
		*
		* @param init The initial state value
		* @param step A function that takes a state and an input, returning a new state
		*/
	def zipWithState[S, E](init: S, step: (S, E) => S)(implicit ec: ExecutionContext): Enumeratee[E, (S, E)] = {
		Enumeratee.scanLeft[E].apply[(S, Option[E])](init -> Option.empty[E]){
			case ((state, _), next) => step(state, next) -> Some(next)
		} compose Enumeratee.collect {
			case (state, Some(event)) => state -> event
		}
	}

	trait StateAccumulator[S, E] {
		def init: S
		def update(state: S, event: E): S
	}

	/** Creates an enumeratee that pairs its inputs with an accumulated state.
		*
		* @param s An accumulator object whose `init` value will be used as the
		*          first state, and whose `update` function will be used to step
		*          from one state to the next given an input.
		*/
	def zipWithState[S, E](s: StateAccumulator[S, E])(implicit ec: ExecutionContext): Enumeratee[E, (S, E)] = {
		zipWithState(s.init, s.update)
	}

	/** Creates an enumeratee that prepends the given value
		*
		* @param toPrepend The value to prepend
		*/
	def prepend[T](toPrepend: T) = new Enumeratee[T, T] {
		def applyOn[A](inner: Iteratee[T, A]): Iteratee[T, Iteratee[T, A]] = {
			val prepended = Iteratee.flatten(inner.feed(Input.El(toPrepend)))
			Enumeratee.passAlong[T](prepended)
		}
	}

	/** Intermediate result for `subdivide` and `subdivideOnState`.
		* This object represents a Stream of Streams - by using an
		* Iteratee to reduce each inner stream to a value, the result
		* will represent a stream of values, represented by an Enumeratee.
		*/
	trait Subdivided[E, Ctx] {
		def combineWith[To](makeCombiner: Try[Ctx] => Iteratee[E, To]): Enumeratee[E, Option[To]]
	}

	/** Creates an Enumeratee that divides the incoming stream into substreams of consecutive events
		* that match some context. Events are folded through a state accumulator in order to determine
		* a "state" for each event. Then each state is passed through a context matching function to
		* determine an optional context value for the event. The context matched by the first event in
		* each substream can then be used to create an Iteratee that consumes that substream. The
		* resulting Enumeratee feeds the result of each substream's consumer (Iteratee).
		*
		* @param stateAccum StateAccumulator used to determine some "state" value for each event
		* @param matchContext Context-matching function that will be called on each event's "state".
		* @param ec implicit ExecutionContext used to combine inner Iteratees and Enumeratees
		* @tparam E The event type
		* @tparam State The state type
		* @tparam Ctx The context type
		* @return A `Subdivided` instance that can be combined with an Iteratee, creating an Enumeratee
		*         that produces the results of running that Iteratee on each substream
		*/
	def subdivide[E, State, Ctx](
		stateAccum: StateAccumulator[State, E],
		matchContext: State => Result[Ctx])(
		implicit ec: ExecutionContext
	): Subdivided[E, Ctx] = {

		// enumeratee that pairs each event with an optional context that is matched from an accumulated state
		val withContext = zipWithState(stateAccum) ><> Enumeratee.map{ case (state, e) => matchContext(state) -> e }

		// type alias to clean up some enumeratee definitions
		type ContextEvent = (Result[Ctx], E)

		// enumeratee that takes events while the context result is not `Empty` (including errors)
		val takeMatches = Enumeratee.takeWhile[ContextEvent](!_._1.isEmpty) ><> Enumeratee.map(_._2)

		// iteratee that ignores all events until a context is provided
		val ignoreNonMatches = Enumeratee.takeWhile[ContextEvent](_._1.isEmpty) &>> Iteratee.ignore

		new Subdivided[E, Ctx] {
			def combineWith[To](makeCombiner: Try[Ctx] => Iteratee[E, To]): Enumeratee[E, Option[To]] = {

				val consumeSubstream = for {
					_ <- ignoreNonMatches
					firstMatch <- Enumeratee.take[ContextEvent](1) &>> Iteratee.head
					results <- firstMatch match {
						case Some((contextResult, event)) if !contextResult.isEmpty =>
							val combiner = makeCombiner(contextResult.toTry)
							takeMatches ><> prepend(event) &>> combiner.map(Some(_))
						case _ =>
							Done[ContextEvent, Option[To]](None)
					}
					_ <- ignoreNonMatches
				} yield results

				withContext ><> Enumeratee.grouped(consumeSubstream)
			}
		}
	}

	/** StateAccumulator that uses the latest event as its state,
		* ignoring any previous 'state' value.
		*
		* @tparam E the event type
		*/
	class IdentityStateAccumulator[E] extends StateAccumulator[E, E] {
		def init = null.asInstanceOf[E]
		def update(state: E, event: E) = event
	}

	/** Simplified case of `subdivide`, where the "state" is ignored, and the "context" is
		* defined by whichever events match the given `filter`. When using `combineWith`, the
		* context will be whichever event happens to be first in that substream.
		*
		* @param filter
		* @param ec
		* @tparam E
		* @return
		*/
	def subdivide[E](filter: E => Boolean)(implicit ec: ExecutionContext): Subdivided[E, E] = {
		val stateAccum = new IdentityStateAccumulator[E]
		val matchContext = Result(_: E).filter(filter)
		subdivide(stateAccum, matchContext)
	}

	case class OpenTag(name: QName, attrs: Map[QName, String])
	type TagStack = List[OpenTag]

	/** State accumulator that keeps track of the open elements and their attributes.
		* The outermost element will be the head of the list of tags, aka the TagStack.
		* E.g. {{{<library size="3"> :: <book id="abc"> :: Nil}}}
		*/
	object TagStackAccumulator extends StateAccumulator[TagStack, XMLEvent] {
		def init = Nil

		def update(stack: TagStack, event: XMLEvent) = event match {
			// start element => push to stack
			case StartElement(name, attrs) =>
				val tag = OpenTag(name, attrs.toMap)
				stack :+ tag

			// end element => pop from stack
			case EndElement(_) =>
				stack dropRight 1

			// any other event => no-op
			case _ => stack
		}
	}

	/** `subdivide` variant for XMLEvents that uses an accumulated `TagStack` as the input
		* to the context matcher.
		*
		* @param matchContext
		* @param ec
		* @tparam C
		* @return
		*/
	def subdivideXml[C](matchContext: TagStack => Result[C])(implicit ec: ExecutionContext): Subdivided[XMLEvent, C] = {
		subdivide(TagStackAccumulator, matchContext)
	}

	/** Represents a current `result`, and a count of previous results that were Errors.
		*
		* @param result The current Result value
		* @param numErrorsEmitted The number of "previous" errors
		* @tparam A The result type
		*/
	case class ErrorCountState[A](result: Result[A], numErrorsEmitted: Int = 0)

	/** An Enumeratee that accumulates an error count for results passed through it.
		* The `numErrorsEmitted` counter will increment **after** a state with an error
		* result. In this manner, the first error passed through will be accompanied by
		* `numErrorsEmitted = 0`, but the very next event will have `numErrorsEmitted = 1`.
		* This behavior can be used in combination with a `TakeWhile` enumeratee to limit
		* the number of errors accepted by the downstream consumers, or kill the stream
		* at (or just after) an error.
		*
		* @tparam A The type of the results being passed through
		*/
	def foldErrorCounts[A]: Enumeratee[Result[A], ErrorCountState[A]] = {
		Enumeratee.scanLeft[Result[A]](ErrorCountState[A](Empty)){ (state, next) =>
			val incErrorCount = if(state.result.isError) 1 else 0
			ErrorCountState(next, state.numErrorsEmitted + incErrorCount)
		}
	}

	/** An Enumeratee that feeds through `Result`s of type `A` until just
		* after the `n`th `Error` result. In other words, streams transformed
		* by this Enumeratee will contain **at most** `n` Errors, and will end
		* after the `n`th error.
		*
		* @param n The number of errors to be passed through before ending the stream
		* @tparam A The type of Results being passed through
		*/
	def takeThroughNthError[A](n: Int)(implicit ec: ExecutionContext) = foldErrorCounts[A]
			.compose(Enumeratee.takeWhile(_.numErrorsEmitted < n))
			.compose(Enumeratee.map(_.result))

	/** Shortcut for `takeThroughNthError(1)`.
		* An Enumeratee that feeds through `Result`s of type `A` until just
		* after the 1st `Error` result.
		* @tparam A The type of Results being passed through
		*/
	def takeThroughFirstError[A](implicit ec: ExecutionContext) = takeThroughNthError[A](1)

	/** An Enumeratee that feeds through `Result`s of type `A` until just
		* before the `n`th `Error` result. In other words, streams transformed
		* by this Enumeratee will contain **less than** `n` Errors, and will end
		* before passing through the `n`th error.
		*
		* @param n The error count threshold before the stream is stopped
		* @tparam A The type of Results being passed through
		*/
	def takeUntilNthError[A](n: Int)(implicit ec: ExecutionContext) = foldErrorCounts[A]
			.compose(Enumeratee.takeWhile{ s => !(s.result.isError && s.numErrorsEmitted >= n - 1)})
			.compose(Enumeratee.map(_.result))

	/** Shortcut for `takeUntilNthError(1)`.
		* An Enumeratee that feeds through `Result`s of type `A`, treating the
		* first `Error` result as an EOF.
		* @tparam A Type type of Results being passed through
		*/
	def takeUntilFirstError[A](implicit ec: ExecutionContext) = takeUntilNthError[A](1)

	// Wrap all of the inputs as a `Some`, then pass a `None` just before the EOF
	private def wrapEOF[A](implicit ec: ExecutionContext): Enumeratee[A, Option[A]] = {
		Enumeratee.mapInputFlatten[A].apply[Option[A]] {
			case Input.EOF => Enumerator.enumInput(Input.El[Option[A]](None)) >>> Enumerator.enumInput(Input.EOF)
			case input => Enumerator.enumInput(input.map(Some(_)))
		}
	}

	/** Create a new Enumeratee that advances a "state" based on the values of the
		* incoming results. Empty inputs will be ignored. Error results will be passed
		* through, but will cause the state to be reset to the folder's initial state.
		*
		* @param s A `StreamScan` that folds values of type `A` into a State,
		*          producing results of type `B`.
		* @tparam A The type of `Result` accepted by the returned Enumeratee
		* @tparam B The type of `Result` generated by the returned Enumeratee
		*/
	def scanResultsWith[A, B](s: StreamScan[A, B])(implicit ec: ExecutionContext) = {
		type ORA = Option[Result[A]]
		wrapEOF[Result[A]] ><> Enumeratee.scanLeft[ORA].apply[(s.State, Result[B])](s.init -> Empty) { (stateAndResult, next) =>
			val (state, _) = stateAndResult // the _2 is the previously-emitted result, and can be ignored now
			next match {
				// no change to the state for empty values
				case Some(Empty) => state -> Empty
				// reset the fold to `f.init`, and emit the error
				case Some(e: Error) => s.init -> e
				// fold the input into the state
				case Some(Success(input)) => s.fold(state, input)
				case None => s.init -> s.finish(state)
			}
		} ><> Enumeratee.map(_._2)
	}

	/** Create a new Enumeratee that advances a "state" based on incoming results.
		* Errors and Empty results will be ignored, while Success results will have
		* their values passed through the given `StreamScan` to produce new results.
		* When the stream reaches an EOF, any remaining state will be 'finished' to
		* potentially create one final result.
		* @param s A `StreamScan` that folds values of type `A` into a State,
		*          producing results of type `B`.
		* @tparam A The type of values that go into the StreamScan
		* @tparam B The type of results that come out of the StreamScan
		* @return
		*/
	def scanWith[State, A, B](s: StreamScan[ A, B])(implicit ec: ExecutionContext) = {
		val collectSuccesses = Enumeratee.collect[Result[A]] { case Success(a) => a }
		val scanner = Enumeratee.scanLeft[Option[A]].apply[(s.State, Result[B])](s.init -> Empty) { (prev, next) =>
			val (state, _) = prev
			next match {
				case Some(input) => s.fold(state, input)
				case None => s.init -> s.finish(state)
			}
		}
		collectSuccesses ><> wrapEOF ><> scanner ><> Enumeratee.map(_._2)
	}

	def consumeSingle[A](implicit ec: ExecutionContext): Iteratee[Result[A], Result[A]] = {
		Iteratee.head map { headOpt => headOpt getOrElse Empty }
	}

	def consumeOptional[A](implicit ec: ExecutionContext): Iteratee[Result[A], Result[Option[A]]] = {
		Iteratee.head map {
			case None => Success(None)
			case Some(Empty) => Success(None)
			case Some(headResult) => headResult.map(Some(_))
		}
	}

	def consumeList[A](implicit ec: ExecutionContext): Iteratee[Result[A], Result[List[A]]] = {
		Iteratee.getChunks map { chunks =>
			Result.list(chunks)
		}
	}

	def consumeConcat[A, B, That](
		implicit ec: ExecutionContext, t: A => TraversableOnce[B], bf: CanBuildFrom[A, B, That]
	): Iteratee[Result[A], Result[That]] = {
		consumeList[A] map { listResult =>
			listResult map { list =>
				val builder = bf()
				list foreach (builder ++= _)
				builder.result()
			}
		}
	}

	def runSideEffect[A](thunk: Result[A] => Unit)(implicit ec: ExecutionContext): Iteratee[Result[A], Result[Unit]] = {
		Iteratee.foreach(thunk) map Success.apply
	}
}
