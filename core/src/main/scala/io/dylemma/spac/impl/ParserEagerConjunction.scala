package io.dylemma.spac
package impl

/** A group of parsers that have been `and`-ed together.
  *
  * This parser will return early with `false` if any of the inner parsers returns `false`.
  * Otherwise, it returns `true` once all of the inner parsers have returned `true`.
  *
  * @param parsers The conjunction group of parsers
  * @tparam In event/input type
  */
case class ParserEagerConjunction[In](parsers: Vector[Parser[In, Boolean]]) extends Parser[In, Boolean] {
	def newHandler: Parser.Handler[In, Boolean] = new Parser.Handler[In, Boolean] {
		private val handlers = parsers.view.map(_.newHandler).toArray
		private var numRunning = handlers.length

		// at EOF, force all of the remaining verifiers to finish, and succeed only if all of them succeeded
		def finish() = handlers.view.take(numRunning).map(_.finish()).forall(identity)

		def step(in: In) = {
			// Call `.step` on each handler, and exit early if it returns false.
			// If a handler returns a `true` result, remove it from the list so we don't need to bother with it anymore.
			// If a handler returns a `Right`, place the new handler in the list in its place.
			// If all handlers returned `Left(true)` during this step, it means the overall verification succeeded.
			ArrayHelper.filterMapInPlace(handlers, numRunning)(_.step(in) match {
				case Left(false) => Left(false)
				case Left(true) => Right(None)
				case Right(h2) => Right(Some(h2))
			}) match {
				case Left(result) => Left(result)
				case Right(0) => Left(true)
				case Right(newLength) =>
					numRunning = newLength
					Right(this)
			}

		}
	}
}
