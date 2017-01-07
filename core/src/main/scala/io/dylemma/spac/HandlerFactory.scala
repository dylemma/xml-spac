package io.dylemma.spac

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
trait HandlerFactory[-In, +Out] extends (Any => HandlerFactory[In, Out]){
	def makeHandler(): Handler[In, Out]

	/** Always returns `this` */
	def apply(v1: Any): HandlerFactory[In, Out] = this
}

object HandlerFactory {
	def apply[In, Out](mh: => Handler[In, Out]): HandlerFactory[In, Out] = new HandlerFactory[In, Out] {
		def makeHandler(): Handler[In, Out] = mh
	}
}