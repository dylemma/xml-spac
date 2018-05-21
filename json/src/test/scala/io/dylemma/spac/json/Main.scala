package io.dylemma.spac.json

import io.dylemma.spac.handlers.ContextMove
import io.dylemma.spac.{BaseStackSplitter, Consumer, ContextMatcher, Splitter, debug}

object Main {
	def main(args: Array[String]): Unit = {
		// debug.enabled.set(true)
		val rawJson =
			"""{
			  |  "hello": [
			  |    {"a": 1 },
			  |    [1, 2, 3],
			  |    true
			  |  ],
			  |  "world": null
			  |}""".stripMargin

		val consumer = Splitter("hello" \ anyIndex).as(Consumer.toList).consumeForEach { events =>
			println(events.mkString("Events[ ", " - ", " ]"))
		}
		consumer consume rawJson

	}

	// Below here is just some brainstorming for how JSON parsers/splitters should
	// be put together in order to handle certain input scenarios.

	/*
	Parser.forInt
	 */
	val j1 =
		"""1"""

	/*
	Parser.forString
	 */
	val j2 =
		""""hello""""

	/*
	Parser.forNull
	-
	Parser.optional(Parser.forInt)
	-
	Parser.forInt.orNull // maybe?
	 */
	val j3 = "null"

	/*
	Splitter(inArray).asListOf(Parser.forInt)
	// inArray = atIndex(_ => true)
	 */
	val j4 = "[1, 2, 3]"

	/*
	(
		Splitter(atIndex(0)).first[Int] ~
		Splitter(atIndex(1)).first[String]
	).asTuple
	 */
	val j5 =
		"""[1, "hello"]"""

	/*
	Splitter(inObject)
	-
	Parser.fieldOpt("a", Parser.forInt)
	 */
	val j6 =
		"""{ }"""

	/*
	Parser.field("a", Parser.forInt)
	-
	Splitter("a").first[Int]
	 */
	val j7 =
		"""{ "a": 1 }"""

	/*
	Parser.forDouble
	 */
	val j8 =
		"""1.234"""

	/*
	Parser.forBool
	 */
	val j9 =
		"""true"""

	/*
	Splitter("a" \ "b" \ atIndex(1)).first[String]
	 */
	val j10 =
		"""{
		  |  "a": {
		  |    "b": [null, "important info", null]
		  |  }
		  |}
		""".stripMargin
}
