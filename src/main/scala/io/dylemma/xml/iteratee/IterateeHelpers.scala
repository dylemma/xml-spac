package io.dylemma.xml.iteratee

import scala.concurrent.ExecutionContext

import play.api.libs.iteratee.{ Iteratee, Enumeratee }

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

	/** Intermediate result for `subdivide` and `subdivideOnState`.
		* This object represents a Stream of Streams - by using an
		* Iteratee to reduce each inner stream to a value, the result
		* will represent a stream of values, represented by an Enumeratee.
		*/
	trait Subdivided[E] {
		def combineWith[To](combiner: Iteratee[E, To]): Enumeratee[E, To]
	}

	/** Creates an Enumeratee that divides its input into substreams,
		* where each substream is a sequence of consecutive values that
		* match the `filter` function. The result of this function must
		* be applied to an Iteratee which convert each substream to a
		* discrete value.
		*
		* @param filter A filter function that decides whether a value
		*               will be included in one of the substreams
		*/
	def subdivide[E](filter: E => Boolean)(implicit ec: ExecutionContext): Subdivided[E] = {
		val takeMatches = Enumeratee.takeWhile[E](filter)
		val takeNonMatches = Enumeratee.takeWhile[E](!filter(_))

		new Subdivided[E] {
			def combineWith[To](folder: Iteratee[E, To]) = Enumeratee.grouped {
				for {
					_ <- takeNonMatches &>> Iteratee.ignore
					results <- takeMatches ><> Enumeratee.mapInput[E]{e => println(e); e} &>> folder
					_ <- takeNonMatches &>> Iteratee.ignore
				} yield results
			}
		}
	}

	/** Similar to `subdivide`, this method creates an Enumeratee that divides its input
		* input substreams, but the filter runs on a state that is accumulated over each input.
		*
		* @param s A state accumulator that will be used to calculate a state for each input
		* @param filter A filter function that decides whether an input with a given state
		*               should be included in one of the substreams
		*/
	def subdivideOnState[E, S](s: StateAccumulator[S, E], filter: S => Boolean)(implicit ec: ExecutionContext): Subdivided[E] = {
		val withState = zipWithState(s)
		val takeMatches = Enumeratee.takeWhile[(S, E)]{ case (s, _) => filter(s) }
		val takeNonMatches = Enumeratee.takeWhile[(S, E)]{ case (s, _) => !filter(s) }
		new Subdivided[E] {
			def combineWith[To](folder: Iteratee[E, To]) = withState ><> Enumeratee.grouped {
				for {
					_ <- takeNonMatches &>> Iteratee.ignore
					results <- takeMatches ><> Enumeratee.map(_._2) &>> folder
					_ <- takeNonMatches &>> Iteratee.ignore
				} yield results
			}
		}
	}
}
