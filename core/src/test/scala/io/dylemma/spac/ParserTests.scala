package io.dylemma.spac

import javax.xml.stream.XMLStreamException

import io.dylemma.spac.Result.{Error, Success}
import org.scalatest.{FunSpec, Matchers}

class ParserTests extends FunSpec with Matchers {

	protected def testParserResult[R](rawXml: String, parser: Parser[Any, R], expected: Result[R]) = {
		val result = parser parse rawXml
		result should be(expected)
	}

	protected def testParserResultLike[R](rawXml: String, parser: Parser[Any, R])(testResult: Result[R] => Boolean) = {
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
				Parser.forMandatoryAttribute("a")
			)(_.isError)
		}

		it("should not return the attribute from an inner element") {
			testParserResultLike[String](
				"""<foo><bar a="123"/></foo>""",
				Parser.forMandatoryAttribute("a")
			)(_.isError)
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

	describe("Invalid XML") {
		it("should cause an exception to be thrown while running a parser") {
			val invalidXml = """<doop oh hello this is just gibberish"""
			val anyOldParser = Parser.forText
			intercept[XMLStreamException] {
				anyOldParser parse invalidXml
			}
		}
	}

	describe("Parser#map") {
		it("should produce successful mapped results for valid inputs") {
			testParserResult(
				"<foo>123</foo>",
				Parser.forText.map(_.toInt),
				Success(123)
			)
		}

		it("should produce error results for invalid inputs") {
			testParserResultLike[Int]("<foo>ABC</foo>", Parser.forText.map(_.toInt))(_.isError)
		}
	}

	describe("Splitter") {
		it("should filter out unmached events"){
			testParserResult(
				"<foo><bar>Hello</bar><baz>World</baz></foo>",
				Splitter(* \ "bar").through(Parser.forText).parseToList,
				Success(List("Hello"))
			)
		}

		it("should split the events into substreams"){
			val rawXml = "<foo><bar>Hello</bar><bar>World</bar></foo>"
			val splitParser = Splitter(* \ "bar").through(Parser.forText).parseToList
			val unsplitParser = Parser.forText
			testParserResult(rawXml, unsplitParser, Success("HelloWorld"))
			testParserResult(rawXml, splitParser, Success(List("Hello", "World")))
		}
	}

	describe("Parser.combine") {
		it("should combine successful results of the combined parsers"){
			val rawXml = """<foo a="123">hello world</foo>"""
			val preCombined = Parser.combine(
				Parser.forMandatoryAttribute("a"),
				Parser.forText
			)

			testParserResult(rawXml, preCombined.asTuple, Success("123" -> "hello world"))
			testParserResult(rawXml, preCombined.as(_ + _), Success("123hello world"))
		}

		it("should result in an Error when any of the combined parsers does"){
			testParserResultLike(
				"""<foo a="abc">hello world</foo>""",
				Parser.combine(
					Parser.forMandatoryAttribute("a").map(_.toInt),
					Parser.forText
				).asTuple
			)(_.isError)
		}

		it("should forbid combination of parsers with conflicting context types"){
			val intContextParser = Parser.forContext[Int]
			val stringContextParser = Parser.forContext[String]
			assertCompiles("Parser.combine(intContextParser, intContextParser)")
			assertDoesNotCompile("Parser.combine(intContextParser, stringContextParser)")
		}

		it("should pick the most-specific context type when combining parsers"){
			class A
			class B extends A
			class C extends B
			val abcParser = Parser.combine(
				Parser.forContext[A],
				Parser.forContext[B],
				Parser.forContext[C]
			).asTuple
			assertDoesNotCompile("val x: Parser[A, (A, B, C)] = abcParser")
			assertDoesNotCompile("val x: Parser[B, (A, B, C)] = abcParser")
			assertCompiles("val x: Parser[C, (A, B, C)] = abcParser")
		}

		it("should pass the same context value to its inner parsers if it requires a context"){
			class A
			case class AText(a: A, text: String)
			val splitter: XmlSplitter[A] = Splitter(attr("a").map(_ => new A))
			val rawXml = """<foo a="123"><x>Hello</x><y>Goodbye</y></foo>"""
			// the two inner parsers should receive the same 'A' instance passed to this parser from a splitter
			val combinedContextualParser: Parser[A, (AText, AText)] = Parser.combine(
				Parser.combine(Parser.forContext[A], Splitter(* \ "x").first.asText).as(AText),
				Parser.combine(Parser.forContext[A], Splitter(* \ "y").first.asText).as(AText)
			).asTuple
			testParserResultLike(rawXml, splitter.first(combinedContextualParser)){
				case Success((x, y)) => x.a === y.a
				case _ => false
			}
		}
	}

	describe("Parser.forContext") {
		it("should use the context provided from the Splitter"){
			val splitter = Splitter(attr("a") \ "bar")
			val barParser = Parser.combine(
				Parser.forContext[String],
				Parser.forText
			).asTuple
			val parser = splitter.through(barParser).parseFirst

			testParserResult(
				"""<foo a="abc"><bar>Hello</bar></foo>""",
				parser,
				Success("abc" -> "Hello")
			)
			testParserResult(
				"""<foo a="def"><bar>Hello</bar></foo>""",
				parser,
				Success("def" -> "Hello")
			)
		}

		it("should result in an Error when the context match throws an exception"){
			testParserResultLike[Int](
				"""<foo a="abc"><bar>Hello</bar></foo>""",
				Splitter(attr("a").map(_.toInt)).through(Parser.forContext[Int]).parseFirst
			)(_.isError)
		}

		it("should forbid parsers with a different context type from being attached"){
			val splitter: XmlSplitter[String] = Splitter(attr("a"))
			val parserString: Parser[String, String] = Parser.forContext[String]
			val parserInt: Parser[Int, Int] = Parser.forContext[Int]
			assertCompiles("splitter through parserString")
			assertDoesNotCompile("splitter through parserInt")
		}
	}

	describe("Parser.choose"){
		val rawXml = """<foo>
		|	<a>1</a>
		|	<a>2</a>
		|	<b>3</b>
		|	<a>4</a>
		|	<b>5</b>
		|</foo>""".stripMargin
		val splitter = Splitter("foo" \ extractElemName)

		sealed trait AB
		case class A(i: Int) extends AB
		case class B(i: Int) extends AB


		it("should choose the right sub-parser based on the context"){
			val abParser = Parser.choose[String] {
				case "a" => Parser.forText.map(s => A(s.toInt))
				case "b" => Parser.forText.map(s => B(s.toInt))
			}

			testParserResultLike(rawXml, (splitter through abParser).parseToList){
				case Success(A(1) :: A(2) :: B(3) :: A(4) :: B(5) :: Nil) => true
				case _ => false
			}
		}

		it("should yield error results where the chooser function fails"){
			val onlyAParser = Parser.choose[String] {
				case "a" => Parser.forText.map(s => A(s.toInt))
				// omit case "b" for errors
			}
			val parser = splitter.through(onlyAParser).expandResults.parseToList
			testParserResultLike(rawXml, parser){
				case Success(List(Success(A(1)), Success(A(2)), Error(_), Success(A(4)), Error(_))) => true
				case _ => false
			}
		}
	}
}