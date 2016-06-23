package io.dylemma.xsp

import org.scalatest.{FunSpec, Matchers}
import io.dylemma.xsp.Result.{ Empty, Error, Success }
import io.dylemma.xsp.syntax._

class XSPTests extends FunSpec with Matchers {

	protected def testParserResult[R](rawXml: String, parser: Parser[Any, R], expected: Result[R]) = {
		val result = parser parse rawXml
		result should be(expected)
	}

	protected def testParserResultLike[R](rawXml: String, parser: Parser[Any, R], testResult: Result[R] => Boolean) = {
		val result = parser parse rawXml
		testResult(result) should be(true)
	}

	describe("Parser.forText") {
		it("should concatenate text events") {
			testParserResult(
				"<foo>Hello<bar>World</bar><bar>Floopy</bar>Doop</foo>",
				Parser.forText,
				Success("HelloWorldFloopyDoop")
			)
		}

		it("should preserve whitespace") {
			testParserResult(
				"<foo>\n\tHello\n</foo>",
				Parser.forText,
				Success("\n\tHello\n")
			)
		}
	}

	describe("Parser.forMandatoryAttribute") {
		it("should return the attribute from the first encountered element") {
			testParserResult(
				"""<foo a="123"/>""",
				Parser.forMandatoryAttribute("a"),
				Success("123")
			)
		}

		it("should return an error if the attribute is missing from the first encountered element") {
			testParserResultLike[String](
				"<foo/>",
				Parser.forMandatoryAttribute("a"),
				{ _.isError }
			)
		}

		it("should not return the attribute from an inner element") {
			testParserResultLike[String](
				"""<foo><bar a="123"/></foo>""",
				Parser.forMandatoryAttribute("a"),
				{ _.isError }
			)
			testParserResult(
				"""<foo a="123"><bar a="456"/></foo>""",
				Parser.forMandatoryAttribute("a"),
				Success("123")
			)
		}
	}

	describe("Parser.forOptionalAttribute") {
		it("should return the attribute from the first encountered element, in a Some") {
			testParserResult(
				"""<foo a="123"/>""",
				Parser.forOptionalAttribute("a"),
				Success(Some("123"))
			)
		}

		it("should return None if the attribute is missing from the first encountered element") {
			testParserResult(
				"<foo/>",
				Parser.forOptionalAttribute("a"),
				Success(None)
			)
		}

		it("should not return the attribute from an inner element") {
			testParserResult(
				"""<foo><bar a="123"/></foo>""",
				Parser.forOptionalAttribute("a"),
				Success(None)
			)
			testParserResult(
				"""<foo a="123"><bar a="456"/></foo>""",
				Parser.forOptionalAttribute("a"),
				Success(Some("123"))
			)
		}
	}
}