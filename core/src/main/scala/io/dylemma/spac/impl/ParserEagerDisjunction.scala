package io.dylemma.spac
package impl

/** A group of parsers that have been `or`-ed together.
  *
  * This parser will return early with `true` as soon as any of the inner parsers returns `true`.
  * Otherwise, it returns `false` once all of the inner parsers have finished with `false`.
  *
  * @param parsers The disjunction group of parsers
  * @tparam In event/input type
  */
case class ParserEagerDisjunction[In](parsers: Vector[Parser[In, Boolean]]) extends Parser[In, Boolean] {
	def newHandler: Parser.Handler[In, Boolean] = new Parser.Handler[In, Boolean] {
		private val handlers = parsers.view.map(_.newHandler).toArray
		private var numRunning = handlers.length

		// at EOF, force all of the remaining verifiers to finish, and succeed if any of them succeeded
		def finish() = handlers.view.take(numRunning).map(_.finish()).exists(identity)

		def step(in: In) = {
			// Call `.step` on each handler, and exit early if it returns true.
			// If a handler returns a `false` result, remove it from the list so we don't need to bother with it anymore.
			// If a handler returns a `Right`, place the new handler in the list in its place.
			// If all handlers returned `Left(false)` during this step, it means the overall verification failed.
			ArrayHelper.filterMapInPlace(handlers, numRunning)(_.step(in) match {
				case Left(true) => Left(true)
				case Left(false) => Right(None)
				case Right(h2) => Right(Some(h2))
			}) match {
				case Left(result) => Left(result)
				case Right(0) => Left(false)
				case Right(newLength) =>
					numRunning = newLength
					Right(this)
			}
		}
	}
}
