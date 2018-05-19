package io.dylemma.spac

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

/** Typeclass for parser-like classes that can be instantiated from a HandlerFactor of a specific input type.
  *
  * @tparam In
  * @tparam T
  */
trait FromHandlerFactory[+In, T[+_]] {
	def makeInstance[Out](hf: HandlerFactory[In, Out], debugName: String): T[Out]
}

object FromHandlerFactory {
	implicit def toConsumer[In]: FromHandlerFactory[In, ({ type T[+out] = Consumer[In, out] })#T] = new FromHandlerFactory[In, ({ type T[+out] = Consumer[In, out] })#T] {
		def makeInstance[Out](hf: HandlerFactory[In, Out], debugName: String): Consumer[In, Out] = new Consumer[In, Out] {
			def makeHandler() = hf.makeHandler()
			override def toString = debugName
		}
	}
}