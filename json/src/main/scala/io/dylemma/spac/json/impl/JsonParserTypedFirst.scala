package io.dylemma.spac
package json
package impl

class JsonParserTypedFirst[A](expected: String, f: JsonEvent => Option[A]) extends Parser[JsonEvent, A] {
	def newHandler = new JsonParserTypedFirst.Handler(expected, f)
}
object JsonParserTypedFirst {
	class Handler[A](expected: String, f: JsonEvent => Option[A]) extends Parser.Handler[JsonEvent, A] {

		def step(in: JsonEvent) = {
			f(in) match {
				case Some(a) => Left(a)
				case None => throw new SpacException.UnexpectedInputException(in, expected :: Nil)
			}
		}

		def finish() = throw new SpacException.UnfulfilledInputsException(expected :: Nil)
	}
}
