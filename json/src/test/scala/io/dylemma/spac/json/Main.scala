package io.dylemma.spac.json

import io.dylemma.spac.handlers.ContextMove
import io.dylemma.spac.{BaseStackSplitter, Consumer, ContextMatcher, Splitter, debug}

object Main {
	def main(args: Array[String]): Unit = {
		debug.enabled.set(true)
		val rawJson =
			"""{
			  |  "hello": [1, 2, 3],
			  |  "world": null
			  |}""".stripMargin

		val ctxTracker = new JsonContextTracker

		class FieldContextMatcher(name: String) extends ContextMatcher[JsonStackElem, Unit] {
			def applyChained[B](stack: IndexedSeq[JsonStackElem], offset: Int, avail: Int, next: ContextMatcher[JsonStackElem, B]): Option[(Unit, B)] = {
				if (avail >= 2 && stack(offset) == JsonStackElem.Object && stack(offset + 1) == JsonStackElem.Field(name)) {
					println(s"CONTEXT MATCH for $name")
					next(stack, offset + 2, avail - 2).map(() -> _)
				} else {
					println("no match @ " + stack.slice(offset, offset + avail).mkString("Context[ ", " - ", " ]"))
					None
				}
			}
		}

		val splitter = new BaseStackSplitter(new FieldContextMatcher("hello"))
		val contextCollector = splitter.through(Consumer.toList).consumeForEach { events =>
			println(events.mkString("Events[ ", " - ", " ]"))
		}
		contextCollector consume rawJson

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
