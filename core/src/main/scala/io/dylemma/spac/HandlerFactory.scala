package io.dylemma.spac

import io.dylemma.spac.types.Functor

import scala.language.higherKinds

/** Base trait for Consumers and Parsers.
  *
  * HandlerFactories must be able to create new Handler instances with no input provided.
  *
  * For convenience, every HandlerFactory is also a function that returns itself.
  * This is because `Splitter`'s `through` method expects a `Context => HandlerFactory`,
  * but sometimes the actual context value does not matter. This allows you to call
  * `someSplitter.through(parser)` instead of `someSplitter.through(_ => parser)`
  *
  * @tparam In The generated handler's input type
  * @tparam Out The generated handler's output type
  */
trait HandlerFactory[-In, +Out] extends (Any => HandlerFactory[In, Out]) {
	def makeHandler(): Handler[In, Out]

	/** Always returns `this`, so that `this` can be passed to [[Splitter#through]] */
	def apply(v1: Any): HandlerFactory[In, Out] = this
}

object HandlerFactory {
	def apply[In, Out](mh: => Handler[In, Out]): HandlerFactory[In, Out] = new HandlerFactory[In, Out] {
		def makeHandler(): Handler[In, Out] = mh
	}
}

/** Mixin for [[HandlerFactory]] that adds mapping operations
  *
  * @tparam In The generated handler's input type
  * @tparam Out The generated handler's output type
  * @tparam R A container type for output values, e.g. `Try` or `[[types.Id]]`
  * @tparam Mapped Type constructor for mapped versions of this factory
  *
  * @define hf handler factory
  * @define HF HandlerFactory
  */
abstract class AbstractHandlerFactory[-In, +Out, R[+_]: Functor, Mapped[-_, +_]] extends HandlerFactory[In, R[Out]] {

	/** Create a new $hf whose result containers are transformed by the given function `f`.
	  * For Consumers, which use `Id` as the result container type, `mapResult` is equivalent to `map`.
	  *
	  * @param f The function applied to each result container
	  * @tparam B The mapped result type
	  * @return a new $HF which derives its results by applying `f` to the result containers generated by this $hf
	  */
	def mapResult[B](f: R[Out] => R[B]): Mapped[In, B]

	/** Create a new $hf whose results are transformed by the given function `f`.
	  *
	  * @param f The function to apply to each result
	  * @tparam B The mapped result type
	  * @return A new $hf which derives its results by applying `f` to the results generated by this $hf
	  * @see [[syntax.FunctorSyntax.NestedFunctorOps#mapF]] for when `Out` is a container e.g. Option or List
	  */
	def map[B](f: Out => B): Mapped[In, B] = mapResult(implicitly[Functor[R]].map(_, f))

}