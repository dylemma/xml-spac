package io.dylemma.spac
package json

import cats.effect.SyncIO
import cats.syntax.apply._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

trait JsonParserBehaviors { this: AnyFunSpec with Matchers =>
	def jsonParserWithStringSource(implicit toPullable: ToPullable[SyncIO, String, JsonEvent]) = {

		def basicParser[A](successType: String, parser: JsonParser[A], successInput: String, expected: A): Unit = {
			val failInputs = List(
				"long" -> "1",
				"double" -> "1.2",
				"string" -> "\"hello\"",
				"array" -> "[1,2,3]",
				"object" -> "{ \"a\": 3 }",
				"null" -> "null",
				"bool" -> "true"
			).filterNot(_._1 == successType)

			it(s"should succeed on a $successType input") {
				parser.parse(successInput) shouldEqual expected
			}

			for ((name, input) <- failInputs) it(s"should fail on a $name input") {
				intercept[IllegalArgumentException] {
					parser parse input
				}
			}
		}

		describe("JsonParser[Int]") {
			it should behave like basicParser("long", JsonParser[Int], "1", 1)
		}
		describe("JsonParser[Long]") {
			it should behave like basicParser("long", JsonParser[Long], "1", 1L)
		}
		describe("JsonParser[Float]") {
			it should behave like basicParser("double", JsonParser[Float], "1.2", 1.2f)
		}
		describe("JsonParser[Double]") {
			it should behave like basicParser("double", JsonParser[Double], "1.2", 1.2)
		}
		describe("JsonParser[String]") {
			it should behave like basicParser("string", JsonParser[String], "\"hello\"", "hello")
		}
		describe("JsonParser[Boolean]") {
			it should behave like basicParser("bool", JsonParser[Boolean], "true", true)
		}
		describe("JsonParser.forNull") {
			it should behave like basicParser("null", JsonParser.forNull, "null", None)
		}

		describe("JsonParser.listOf") {
			it should behave like basicParser("array", JsonParser.listOf[Int], "[1,2,3]", List(1, 2, 3))

			it("should succeed when given an array as input") {
				JsonParser.listOf[Int].parse("[1,2,3]") should be(List(1, 2, 3))
				JsonParser.listOf[Boolean].parse("[true, false]") should be(List(true, false))
				JsonParser.listOf[String].parse("[\"hi\", \"bye\"]") should be(List("hi", "bye"))
			}
		}

		describe("JsonParser.objectOf") {
			it should behave like basicParser("object", JsonParser.objectOf[Int], """{"a": 1, "b": 2}""", Map("a" -> 1, "b" -> 2))
			it("should work properly when the inner parser is complex") {
				val json =
					"""{
					  | "x": {
					  |  "foo": 3
					  | },
					  | "y": {
					  |  "foo": 4
					  | }
					  |}
				""".stripMargin
				val fooParser = Splitter.json("foo").as[Int].parseFirst
				JsonParser.objectOf(fooParser).parse(json) should be(Map("x" -> 3, "y" -> 4))
			}
		}

		describe("JsonParser.oneOf") {
			it("should succeed if the input causes one of the parsers to succeed") {
				val oneOf = JsonParser.oneOf[Any](JsonParser[Int], JsonParser[String], JsonParser[Boolean])
				oneOf.parse("1") should be(1)
				oneOf.parse("\"hello\"") should be("hello")
				oneOf.parse("false") should equal(false) // compile fail with `be`... scalatest bug?
			}
			it("should fail if the input causes all of the parsers to fail") {
				val oneOf = Parser.oneOf(JsonParser[Int], JsonParser[String], JsonParser[Boolean])
				intercept[SpacException.FallbackChainFailure] {
					oneOf.parse("[1,2,3]")
				}
			}
		}

		describe("JsonSplitter(<object field>)") {
			it("should apply the attached parser to the inputs in the field's scope") {
				val fieldParser = Splitter.json("field").as[Int].parseFirst
				val json = "{ \"field\": 3 }"
				fieldParser.parse(json) shouldEqual 3
			}

			it("should not match nested contexts that would have matched at the same level") {
				val fieldParser = Splitter.json("field").as[Int].parseToList
				val json =
					"""{
					  |  "field": 3,
					  |  "inner": {
					  |    "field": 4
					  |  }
					  |}""".stripMargin
				fieldParser.parse(json) shouldEqual List(3)
			}

			it("should identify matching contexts even if that context was matched before") {
				val fieldParser = Splitter.json("field").as[Int].parseToList
				val json =
					"""{
					  |  "field": 3,
					  |  "field": 4
					  |}""".stripMargin
				fieldParser.parse(json) shouldEqual List(3, 4)
			}
		}

		describe("JsonSplitter(anyField)") {
			it("should extract values regardless of the field name") {
				val fieldParser = Splitter.json(anyField).as[Int].parseToList
				val json =
					"""{
					  |  "a": 1,
					  |  "b": 2
					  |}""".stripMargin
				fieldParser.parse(json) shouldEqual List(1, 2)
			}

			it("should extract field names") {
				val fieldParser = Splitter.json(anyField).map(Parser.pure).parseToList
				val json = """{ "a": 1, "b": 2 }"""
				fieldParser.parse(json) shouldEqual List("a", "b")
			}
		}

		describe("JsonSplitter(<array index>)") {
			it("should extract values only from matching indexes") {
				val parser = Splitter.json(indexWhere(_ % 2 == 0)).as[Int].parseToList
				val json = "[10, 20, 30, 40, 50]" // indexes 0,2,4 match
				parser.parse(json) shouldEqual List(10, 30, 50)
			}

			it("should extract array indexes") {
				val parser = Splitter.json(indexWhere(_ % 2 == 0)).map(Parser.pure).parseToList
				val json = "[10, 20, 30, 40, 50]" // indexes 0,2,4 match
				parser.parse(json) shouldEqual List(0, 2, 4)
			}
		}

		describe("Combining parsers to create a complex object parser") {
			it("should succeed when an expected input is sent") {
				case class Result(info: List[Int], name: String)
				val json =
					"""{
					  |  "info": [1, 2, 3],
					  |  "name": "Jason"
					  |}""".stripMargin
				val parser = (
					Splitter.json("info").joinBy(JsonParser.listOf[Int]).parseFirst,
					Splitter.json("name").joinBy(JsonParser[String]).parseFirst
					).mapN(Result)
				parser.parse(json) should be(Result(List(1, 2, 3), "Jason"))
			}
		}

		describe("JsonSplitter#firstNotNull") {
			/*it("should skip all null values leading up to the non-null value") {
				val json = "[null, null, 3]"
				JsonSplitter(anyIndex).firstNotNull[Int].parse(json) should be(3)
			}
			it("should fail if there is no non-null value") {
				val json = "[null, null, null]"
				intercept[NoSuchElementException] {
					JsonSplitter(anyIndex).firstNotNull[Int].parse(json)
				}
			}*/
			pending
		}
	}
}
