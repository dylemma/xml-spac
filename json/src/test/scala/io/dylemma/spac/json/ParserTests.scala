package io.dylemma.spac.json

import io.dylemma.spac.Parser
import org.scalatest.{FunSpec, Matchers}

class ParserTests extends FunSpec with Matchers {

	def basicParser[A](successType: Symbol, parser: JsonParser[A], successInput: String, expected: A): Unit = {
		val failInputs = List(
			'long -> "1",
			'double -> "1.2",
			'string -> "\"hello\"",
			'array -> "[1,2,3]",
			'object -> "{ \"a\": 3 }",
			'null -> "null",
			'bool -> "true"
		).filterNot(_._1 == successType)

		it(s"should succeed on a $successType input"){
			parser.parse(successInput) should be(expected)
		}

		for((name, input) <- failInputs) it(s"should fail on a $name input"){
			intercept[IllegalArgumentException] {
				parser parse input
			}
		}
	}

	describe("JsonParser[Int]"){
		it should behave like basicParser('long, JsonParser[Int], "1", 1)
	}
	describe("JsonParser[Long]"){
		it should behave like basicParser('long, JsonParser[Long], "1", 1L)
	}
	describe("JsonParser[Float]"){
		it should behave like basicParser('double, JsonParser[Float], "1.2", 1.2f)
	}
	describe("JsonParser[Double]"){
		it should behave like basicParser('double, JsonParser[Double], "1.2", 1.2)
	}
	describe("JsonParser[String]") {
		it should behave like basicParser('string, JsonParser[String], "\"hello\"", "hello")
	}
	describe("JsonParser[Boolean]"){
		it should behave like basicParser('bool, JsonParser[Boolean], "true", true)
	}
	describe("JsonParser.forNull"){
		it should behave like basicParser('null, JsonParser.forNull, "null", None)
	}

	describe("JsonParser.listOf"){
		it should behave like basicParser('array, JsonParser.listOf[Int], "[1,2,3]", List(1,2,3))

		it("should succeed when given an array as input"){
			JsonParser.listOf[Int].parse("[1,2,3]") should be(List(1,2,3))
			JsonParser.listOf[Boolean].parse("[true, false]") should be(List(true, false))
			JsonParser.listOf[String].parse("[\"hi\", \"bye\"]") should be(List("hi", "bye"))
		}
	}

	describe("Parser.oneOf"){
		it("should succeed if the input causes one of the parsers to succeed"){
			val oneOf = Parser.oneOf(JsonParser[Int], JsonParser[String], JsonParser[Boolean])
			oneOf.parse("1") should be(1)
			oneOf.parse("\"hello\"") should be("hello")
			oneOf.parse("false") should equal(false) // compile fail with `be`... scalatest bug?
		}
		it("should fail if the input causes all of the parsers to fail"){
			val oneOf = Parser.oneOf(JsonParser[Int], JsonParser[String], JsonParser[Boolean])
			intercept[IllegalArgumentException] {
				oneOf.parse("[1,2,3]")
			}
		}
	}

	describe("JsonSplitter(<object field>)"){
		it("should apply the attached parser to the inputs in the field's scope"){
			val fieldParser = JsonSplitter("field").first[Int]
			val json = "{ \"field\": 3 }"
			fieldParser.parse(json) should be(3)
		}

		it("should not match nested contexts that would have matched at the same level"){
			val fieldParser = JsonSplitter("field").asListOf[Int]
			val json =
				"""{
				  |  "field": 3,
				  |  "inner": {
				  |    "field": 4
				  |  }
				  |}""".stripMargin
			fieldParser.parse(json) should be(List(3))
		}

		it("should identify matching contexts even if that context was matched before"){
			val fieldParser = JsonSplitter("field").asListOf[Int]
			val json =
				"""{
				  |  "field": 3,
				  |  "field": 4
				  |}""".stripMargin
			fieldParser.parse(json) should be(List(3,4))
		}
	}

	describe("JsonSplitter(anyField)") {
		it("should extract values regardless of the field name") {
			val fieldParser = JsonSplitter(anyField).asListOf[Int]
			val json =
				"""{
				  |  "a": 1,
				  |  "b": 2
				  |}""".stripMargin
			fieldParser.parse(json) should be(List(1,2))
		}

		it("should extract field names") {
			val fieldParser = JsonSplitter(anyField).asListOf.choose(Parser.constant)
			val json = """{ "a": 1, "b": 2 }"""
			fieldParser.parse(json) should be(List("a", "b"))
		}
	}

	describe("JsonSplitter(<array index>)") {
		it("should extract values only from matching indexes") {
			val parser = JsonSplitter(indexWhere(_ % 2 == 0)).asListOf[Int]
			val json = "[10, 20, 30, 40, 50]" // indexes 0,2,4 match
			parser.parse(json) should be(List(10, 30, 50))
		}

		it("should extract array indexes") {
			val parser = JsonSplitter(indexWhere(_ % 2 == 0)).asListOf.choose(Parser.constant)
			val json = "[10, 20, 30, 40, 50]" // indexes 0,2,4 match
			parser.parse(json) should be(List(0, 2, 4))
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
				JsonSplitter("info").first(JsonParser.listOf[Int]) and
				JsonSplitter("name").first(JsonParser[String])
			).as(Result)
			parser.parse(json) should be(Result(List(1,2,3), "Jason"))
		}
	}
}
