package io.dylemma.spac.json

import io.dylemma.spac.{Consumer, HandlerFactory, Splitter, debug, json}

object Main {

	val helloWorldJson =
		"""{
		  |  "hello": [
		  |    {"a": 1 },
		  |    "wtf is this",
		  |    [1, 2, 3],
		  |    true
		  |  ],
		  |  "world": null
		  |}""".stripMargin

	// ADT representing the kinds of things in the `hello` array
	sealed trait HelloItem
	object HelloItem {
		case class A(value: Int) extends HelloItem
		case class Arr(values: List[Int]) extends HelloItem
		case class Bool(value: Boolean) extends HelloItem
		case class Str(value: String) extends HelloItem
		case class AEvents(events: List[JsonEvent]) extends HelloItem

		implicit val helloParser = Parser.oneOf(
			Splitter("a").first[Int].map(A),
			Splitter(anyIndex).asListOf[Int].map(Arr),
			Parser[String].map(Str),
			Parser[Boolean].map(Bool)
		)
	}

	// case class representing a "hello world" object
	case class HelloWorld(
		hello: List[HelloItem],
		world: Option[String]
	)
	object HelloWorld {
		implicit val helloWorldParser: Parser[HelloWorld] = (
			Splitter("hello" \ anyIndex).asListOf[HelloItem] and
			Splitter("world").first(Parser.nullable[String])
		).as(HelloWorld.apply)
	}


	def main(args: Array[String]): Unit = {
		debug.enabled.set(false)

		println(Parser[HelloWorld] parse helloWorldJson)
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
