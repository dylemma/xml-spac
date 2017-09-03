package io.dylemma.spac

import scala.language.higherKinds

/** An intermediate object for creating sequence-based combination methods for a Parser or Consumer.
  *
  * @tparam M Type constructor for the parser/consumer of a given output type
  * @tparam T1 Output type for the "first" parser/consumer; using the combination methods in this trait
  *            will result in an instance of T1 being used to create a "second" parser/consumer/transformer
  *            to be run sequentially after the first.
  */
trait FollowedBy[M[+_], +T1] { ts =>

	/** Creates a sequence handler by combining this one and a `getNext` function such that when this
	  * handler finishes, a second handler is created by passing its result ot `getNext`.
	  *
	  * @param getNext A function that takes this handler's result to create a second handler
	  * @tparam T2 The output type of the second handler
	  * @return The combined handler
	  */
	def apply[T2](getNext: T1 => M[T2]): M[T2]

	/** Alias for `apply`, to help use this object in for-comprehensions.
	  *
	  * Creates a sequence handler by combining this one and a `getNext` function such that when this
	  * handler finishes, a second handler is created by passing its result ot `getNext`.
	  *
	  * @param getNext A function that takes this handler's result to create a second handler
	  * @tparam T2 The output type of the second handler
	  * @return The combined handler
	  */
	def flatMap[T2](getNext: T1 => M[T2]): M[T2] = apply(getNext)

	/** Convenience for using this object in for-comprehensions; wraps this `ToSequence`
	  * by calling `f` on the first handler's result and passing that into the `getNext` function.
	  * You probably don't want to call this directly
	  */
	def map[T2](f: T1 => T2): FollowedBy[M, T2] = new FollowedBy[M, T2] {
		def apply[T3](getNext: T2 => M[T3]): M[T3] = ts{ n => getNext(f(n)) }
	}
}
