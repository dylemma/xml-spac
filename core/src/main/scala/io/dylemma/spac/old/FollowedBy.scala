package io.dylemma.spac.old

/** An intermediate object for creating sequence-based combination methods for a Parser or Consumer.
  *
  * @tparam M   Type constructor for the parser/consumer of a given output type
  * @tparam Out Output type for the "first" parser/consumer; using the combination methods in this trait
  *             will result in an instance of T1 being used to create a "second" parser/consumer/transformer
  *             to be run sequentially after the first.
  */
trait FollowedBy[-In, +Out, M[- _, + _]] { ts =>

	/** Creates a sequence handler by combining this one and a `getNext` function such that when this
	  * handler finishes, a second handler is created by passing its result ot `getNext`.
	  *
	  * @param getNext A function that takes this handler's result to create a second handler
	  * @tparam T2 The output type of the second handler
	  * @return The combined handler
	  */
	def apply[I2 <: In, T2](getNext: Out => M[I2, T2])(implicit stackable: OldStackable[I2]): M[I2, T2]

	/** Alias for `apply`, to help use this object in for-comprehensions.
	  *
	  * Creates a sequence handler by combining this one and a `getNext` function such that when this
	  * handler finishes, a second handler is created by passing its result ot `getNext`.
	  *
	  * @param getNext A function that takes this handler's result to create a second handler
	  * @tparam T2 The output type of the second handler
	  * @return The combined handler
	  */
	def flatMap[I2 <: In, T2](getNext: Out => M[I2, T2])(implicit stackable: OldStackable[I2]): M[I2, T2] = apply(getNext)

	/** Convenience for using this object in for-comprehensions; wraps this `ToSequence`
	  * by calling `f` on the first handler's result and passing that into the `getNext` function.
	  * You probably don't want to call this directly
	  */
	def map[I2 <: In, T2](f: Out => T2)(implicit stackable: OldStackable[I2]): FollowedBy[In, T2, M] = new FollowedBy[In, T2, M] {
		def apply[I2 <: In, T3](getNext: T2 => M[I2, T3])(implicit stackable: OldStackable[I2]): M[I2, T3] = ts { n => getNext(f(n)) }
	}
}
