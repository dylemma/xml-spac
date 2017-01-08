package io.dylemma.spac

import javax.xml.stream.XMLStreamException

import org.scalatest.{FunSpec, Matchers}

import scala.util.{Failure, Success, Try}

class ParserTests extends FunSpec with Matchers {

	protected def testParserResult[R](rawXml: String, parser: Parser[R], expected: Try[R]) = {
		val result = parser parse rawXml
		result should be(expected)
	}

	protected def testParserResultLike[R](rawXml: String, parser: Parser[R])(testResult: Try[R] => Boolean) = {
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
			)(_.isFailure)
		}

		it("should not return the attribute from an inner element") {
			testParserResultLike[String](
				"""<foo><bar a="123"/></foo>""",
				Parser.forMandatoryAttribute("a")
			)(_.isFailure)
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
			testParserResultLike[Int]("<foo>ABC</foo>", Parser.forText.map(_.toInt))(_.isFailure)
		}
	}

	describe("Parser#mapF (from FunctorSyntax)"){
		val rawXml = """<foo>
		| <node id="1"/>
		| <node id="2"/>
		| <node/>
		| <node id="4"/>
		|</foo>""".stripMargin

		it("should provide a convenient alternate syntax for map(_.map(f))") {
			val idParser = Parser.forOptionalAttribute("id").mapF(_.toInt)
			val allIdsParser = Splitter("foo" \ "node").asListOf(idParser)
			testParserResult(rawXml, allIdsParser, Success(
				List(Some(1), Some(2), None, Some(4))
			))
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

	describe("Parser.and") {
		it("should combine successful results of the combined parsers"){
			val rawXml = """<foo a="123">hello world</foo>"""
			val preCombined = Parser.forMandatoryAttribute("a") and Parser.forText

			testParserResult(rawXml, preCombined.asTuple, Success("123" -> "hello world"))
			testParserResult(rawXml, preCombined.as(_ + _), Success("123hello world"))
		}

		it("should result in an Error when any of the combined parsers does"){
			testParserResultLike(
				"""<foo a="abc">hello world</foo>""",
				(Parser.forMandatoryAttribute("a").map(_.toInt) and Parser.forText).asTuple
			)(_.isFailure)
		}

		it("should pass the same context value to its inner parsers if it requires a context"){
			class A
			case class AText(a: A, text: String)
			val splitter: XmlSplitter[A] = Splitter(attr("a").map(_ => new A))
			val rawXml = """<foo a="123"><x>Hello</x><y>Goodbye</y></foo>"""
			// the two inner parsers should receive the same 'A' instance passed to this parser from a splitter
			def parseAText(context: A, elem: String) = {
				(Parser.constant(context) and Splitter(* \ elem).first.asText).as(AText)
			}
			val combinedContextualParser: Parser[(AText, AText)] = splitter.first{ a =>
				(parseAText(a, "x") and parseAText(a, "y")).asTuple
			}
			testParserResultLike(rawXml, splitter.first(combinedContextualParser)){
				case Success((x, y)) => x.a === y.a
				case _ => false
			}
		}
	}

	describe("Parser.constant"){
		it("should emit the result immediately"){
			testParserResult("<a></a>", Parser.constant(123), Success(123))
		}

		it("should pass errors through instead of the result"){
			val e = new Exception("test error")
			Parser.constant(123).makeHandler().handleError(e) should equal(Some(Failure(e)))
		}

		it("should emit in response to an EOF"){
			Parser.constant(123).makeHandler().handleEnd() should equal(Success(123))
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
			val abTransformer = splitter through {
				case "a" => Parser.forText.map(s => A(s.toInt))
				case "b" => Parser.forText.map(s => B(s.toInt))
			}

			testParserResultLike(rawXml, abTransformer.parseToList){
				case Success(A(1) :: A(2) :: B(3) :: A(4) :: B(5) :: Nil) => true
				case _ => false
			}
		}

		it("should yield error results where the chooser function fails"){
			val onlyATransformer = splitter through {
				case "a" => Parser.forText.map(s => A(s.toInt))
				// omit case "b" for errors
			}
			val parser = onlyATransformer.wrapSafe.parseToList
			testParserResultLike(rawXml, parser){
				case Success(List(Success(A(1)), Success(A(2)), Failure(_), Success(A(4)), Failure(_))) => true
				case _ => false
			}
		}
	}
}