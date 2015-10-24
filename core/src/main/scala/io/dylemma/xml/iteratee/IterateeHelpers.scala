package io.dylemma.xml.iteratee

import javax.xml.namespace.QName
import javax.xml.stream.events.XMLEvent
import scala.concurrent.ExecutionContext

import io.dylemma.xml.event.{ EndElement, StartElement }
import play.api.libs.iteratee.{ Done, Enumeratee, Input, Iteratee }

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
		def combineWith[To](makeCombiner: Ctx => Iteratee[E, To]): Enumeratee[E, Option[To]]

		def combineWith[To](combiner: Iteratee[E, To]): Enumeratee[E, Option[To]] = combineWith(_ => combiner)
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
		matchContext: State => Option[Ctx])(
		implicit ec: ExecutionContext
	): Subdivided[E, Ctx] = {

		// enumeratee that pairs each event with an optional context that is matched from an accumulated state
		val withContext = zipWithState(stateAccum) ><> Enumeratee.map{ case (state, e) => matchContext(state) -> e }

		// type alias to clean up some enumeratee definitions
		type ContextEvent = (Option[Ctx], E)

		// enumeratee that takes events until the context is gone
		val takeMatches = Enumeratee.takeWhile[ContextEvent](_._1.isDefined) ><> Enumeratee.map(_._2)

		// iteratee that ignores all events until a context is provided
		val ignoreNonMatches = Enumeratee.takeWhile[ContextEvent](_._1.isEmpty) &>> Iteratee.ignore

		new Subdivided[E, Ctx] {
			def combineWith[To](makeCombiner: Ctx => Iteratee[E, To]): Enumeratee[E, Option[To]] = {

				val consumeSubstream = for {
					_ <- ignoreNonMatches
					firstMatch <- Enumeratee.take[ContextEvent](1) &>> Iteratee.head
					results <- firstMatch match {
						case Some((Some(context), event)) =>
							val combiner = makeCombiner(context)
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
		val matchContext = Some(_: E).filter(filter)
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
	def subdivideXml[C](matchContext: TagStack => Option[C])(implicit ec: ExecutionContext): Subdivided[XMLEvent, C] = {
		subdivide(TagStackAccumulator, matchContext)
	}
}
