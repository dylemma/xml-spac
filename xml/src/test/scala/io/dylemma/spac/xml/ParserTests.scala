package io.dylemma.spac.xml

import javax.xml.stream.XMLStreamException
import javax.xml.stream.events.XMLEvent

import io.dylemma.spac._
import io.dylemma.spac.xml._
import org.scalatest.{FunSpec, Matchers}

import scala.util.{Failure, Success}

class ParserTests extends FunSpec with Matchers {

	protected def testParserResult[R](rawXml: String, parser: XMLParser[R], expected: R) = {
		val result = parser parse rawXml
		result should be(expected)
	}

	protected def testParserResultLike[R](rawXml: String, parser: XMLParser[R])(testResult: R => Boolean) = {
		val result = parser parse rawXml
		testResult(result) should be(true)
	}

	// convenience for testing behavior of transformers
	protected def runTransformer[Out](xml: String, t: Transformer[XMLEvent, Out]): List[Out] = t.parseToList.parse(xml)

	describe("Parser.forText") {
		it("should concatenate text events") {
			testParserResult(
				"<foo>Hello<bar>World</bar><bar>Floopy</bar>Doop</foo>",
				XMLParser.forText,
				"HelloWorldFloopyDoop"
			)
		}

		it("should preserve whitespace") {
			testParserResult(
				"<foo>\n\tHello\n</foo>",
				XMLParser.forText,
				"\n\tHello\n"
			)
		}
	}

	describe("Parser.forMandatoryAttribute") {
		it("should return the attribute from the first encountered element") {
			testParserResult(
				"""<foo a="123"/>""",
				XMLParser.forMandatoryAttribute("a"),
				"123"
			)
		}

		it("should return an error if the attribute is missing from the first encountered element") {
			an[Exception] should be thrownBy {
				XMLParser.forMandatoryAttribute("a") parse "<foo/>"
			}
		}

		it("should not return the attribute from an inner element") {
			an[Exception] should be thrownBy {
				XMLParser.forMandatoryAttribute("a") parse {
					"""<foo><bar a="123"/></foo>"""
				}
			}

			testParserResult(
				"""<foo a="123"><bar a="456"/></foo>""",
				XMLParser.forMandatoryAttribute("a"),
				"123"
			)
		}
	}

	describe("Parser.forOptionalAttribute") {
		it("should return the attribute from the first encountered element, in a Some") {
			testParserResult(
				"""<foo a="123"/>""",
				XMLParser.forOptionalAttribute("a"),
				Some("123")
			)
		}

		it("should return None if the attribute is missing from the first encountered element") {
			testParserResult(
				"<foo/>",
				XMLParser.forOptionalAttribute("a"),
				None
			)
		}

		it("should not return the attribute from an inner element") {
			testParserResult(
				"""<foo><bar a="123"/></foo>""",
				XMLParser.forOptionalAttribute("a"),
				None
			)
			testParserResult(
				"""<foo a="123"><bar a="456"/></foo>""",
				XMLParser.forOptionalAttribute("a"),
				Some("123")
			)
		}
	}

	describe("Invalid XML") {
		it("should cause an exception to be thrown while running a parser") {
			val invalidXml = """<doop oh hello this is just gibberish"""
			val anyOldParser = XMLParser.forText
			intercept[XMLStreamException] {
				anyOldParser parse invalidXml
			}
		}
	}

	describe("Parser#map") {
		it("should produce successful mapped results for valid inputs") {
			testParserResult(
				"<foo>123</foo>",
				XMLParser.forText.map(_.toInt),
				123
			)
		}

		it("should produce error results for invalid inputs") {
			an[Exception] should be thrownBy {
				XMLParser.forText.map(_.toInt) parse "<foo>ABC</foo>"
			}
		}
	}

	describe("XMLSplitter") {
		it("should filter out unmached events"){
			testParserResult(
				"<foo><bar>Hello</bar><baz>World</baz></foo>",
				XMLSplitter(* \ "bar").map(XMLParser.forText).parseToList,
				List("Hello")
			)
		}

		it("should split the events into substreams"){
			val rawXml = "<foo><bar>Hello</bar><bar>World</bar></foo>"
			val splitParser = XMLSplitter(* \ "bar").map(XMLParser.forText).parseToList
			val unsplitParser = XMLParser.forText
			testParserResult(rawXml, unsplitParser, "HelloWorld")
			testParserResult(rawXml, splitParser, List("Hello", "World"))
		}
	}

	describe("Parser.and") {
		it("should combine successful results of the combined parsers"){
			val rawXml = """<foo a="123">hello world</foo>"""
			val preCombined = XMLParser.forMandatoryAttribute("a") and XMLParser.forText

			testParserResult(rawXml, preCombined.asTuple, "123" -> "hello world")
			testParserResult(rawXml, preCombined.as(_ + _), "123hello world")
		}

		it("should result in an Error when any of the combined parsers does"){
			val parser = (XMLParser.forMandatoryAttribute("a").map(_.toInt) and XMLParser.forText).asTuple
			val xml = """<foo a="abc">hello world</foo>"""
			an[Exception] should be thrownBy {
				parser parse xml
			}
		}

		it("should pass the same context value to its inner parsers if it requires a context"){
			class A
			case class AText(a: A, text: String)
			val splitter: XMLSplitter[A] = XMLSplitter(attr("a").map(_ => new A))
			val rawXml = """<foo a="123"><x>Hello</x><y>Goodbye</y></foo>"""
			// the two inner parsers should receive the same 'A' instance passed to this parser from a splitter
			def parseAText(context: A, elem: String) = {
				(Parser.constant(context) and XMLSplitter(* \ elem).first.asText).as(AText)
			}
			val combinedContextualParser: XMLParser[(AText, AText)] = splitter.first{ a =>
				(parseAText(a, "x") and parseAText(a, "y")).asTuple
			}
			testParserResultLike(rawXml, splitter.first(combinedContextualParser)){
				case (x, y) => x.a === y.a
			}
		}
	}

	describe("Consumer.and <via ConsumerSyntax>"){
		it("should combine results of the combined consumers"){
			val c1 = Parser.first[Int].map(_ * 2)
			val c2 = Parser.first[Int].map(_ * 3)
			val cc = (c1 and c2).asTuple

			cc.parse(List(2)) should be(4 -> 6)
		}
	}

	describe("Parser.constant"){
		it("should emit the result immediately"){
			testParserResult("<a></a>", Parser.constant(123), 123)
		}

		it("should pass errors through instead of the result"){
			val e = new Exception("test error")
			the[Exception] thrownBy {
				Parser.constant(123).makeHandler().handleError(e) should equal(Some(Failure(e)))
			} should be(e)
		}

		it("should emit in response to an EOF"){
			Parser.constant(123).makeHandler().handleEnd() should equal(123)
		}
	}

	describe("XMLSplitter.map"){
		val rawXml = """<foo>
		|	<a>1</a>
		|	<a>2</a>
		|	<b>3</b>
		|	<a>4</a>
		|	<b>5</b>
		|</foo>""".stripMargin
		val splitter = XMLSplitter("foo" \ extractElemName)

		sealed trait AB
		case class A(i: Int) extends AB
		case class B(i: Int) extends AB


		it("should choose the right sub-parser based on the context"){
			val abTransformer = splitter map {
				case "a" => XMLParser.forText.map(s => A(s.toInt))
				case "b" => XMLParser.forText.map(s => B(s.toInt))
			}

			testParserResultLike(rawXml, abTransformer.parseToList){
				case A(1) :: A(2) :: B(3) :: A(4) :: B(5) :: Nil => true
				case _ => false
			}
		}

		/** Upon further thought, it *shouldn't* have this behavior.
		  * Introducing `Try` should be done explicitly by the client,
		  * and in this case, it would be done by protecting the function
		  * being passed to `map` with its own try/catch and a fallback
		  * to a parser that yields Failures.
		  */
		ignore("should yield error results where the chooser function fails"){
			val onlyATransformer = splitter map {
				case "a" => XMLParser.forText.map(s => A(s.toInt))
				// omit case "b" for errors
			}
			val parser = onlyATransformer.wrapSafe.parseToList
			testParserResultLike(rawXml, parser){
				case List(Success(A(1)), Success(A(2)), Failure(_), Success(A(4)), Failure(_)) => true
				case _ => false
			}
		}
	}

	describe("Parser.followedBy"){
		it("should pass the result of the followed parser to create the resulting parser"){
			val xml = "<x><id>123</id><msg>Hello</msg><msg>Goodbye</msg></x>"
			val idParser = XMLSplitter(* \ "id").first.asText
			val msgsParser = idParser.followedBy(id => XMLSplitter(* \ "msg").asText.map(msg => s"$id:$msg").parseToList)
			testParserResult(xml, msgsParser, List("123:Hello", "123:Goodbye"))
		}
		it("should provide convenient flatMap syntax that works the same way"){
			val xml = "<x><id>123</id><msg>Hello</msg><msg>Goodbye</msg></x>"
			val msgsParser = for {
				id <- XMLSplitter(* \ "id").first.asText.followedBy
				msgs <- XMLSplitter(* \ "msg").asText.map(msg => s"$id:$msg").parseToList
			} yield msgs
			testParserResult(xml, msgsParser, List("123:Hello", "123:Goodbye"))
		}
		it("should not pass a result until the followed parser has finished"){
			val xml = "<x><id>123</id><msg>Hello</msg><msg>Goodbye</msg></x>"
			// this parser does not give a result until the containing element ends,
			// so the "following" parser will never receive any events (besides the stack replay)
			val idsParser = XMLSplitter(* \ "id").asText.parseToList.map(_.mkString)
			val msgsParser = idsParser.followedBy(id => XMLSplitter(* \ "msg").asText.map(msg => s"$id:$msg").parseToList)
			testParserResult(xml, msgsParser, Nil)
		}
		it("should yield an error if the followed parser yields an error"){
			val xml = "<x><id>ABC</id><msg>Hello</msg><msg>Goodbye</msg></x>"
			val idParser = XMLSplitter(* \ "id").first.asText.map(_.toInt) // will yield a Failure because of "ABC".toInt
			val msgsParser = idParser.followedBy(id => XMLSplitter(* \ "msg").asText.map(msg => s"$id:$msg").parseToList)

			an[Exception] should be thrownBy {
				msgsParser parse xml
			}
		}
		it("should yield whatever errors the 'following' parser yields"){
			val xml = "<x><name>dylemma</name><num>1</num><num>B</num><num>3</num></x>"
			val nameParser = XMLSplitter(* \ "name").first.asText
			val numsParser = nameParser.followedBy { name =>
				XMLSplitter(* \ "num").asText.map(s => s"$name:${s.toInt}").wrapSafe.parseToList
			}
			testParserResultLike(xml, numsParser){
				case Success("dylemma:1") :: Failure(_) :: Success("dylemma:3") :: Nil => true
				case _ => false
			}
		}

	}

	describe("Parser.followedByStream"){


		it("should pass the result of the followed parser to create the resulting transformer"){
			val xml = "<x><id>123</id><msg>Hello</msg><msg>Goodbye</msg></x>"
			val idParser = XMLSplitter(* \ "id").first.asText
			val msgsStream = idParser.followedByStream{ id => XMLSplitter(* \ "msg").asText.map(msg => s"$id:$msg") }
			runTransformer(xml, msgsStream) should be(List("123:Hello", "123:Goodbye"))
		}
		it("should provide convenient flatMap syntax that works the same way"){
			val xml = "<x><id>123</id><msg>Hello</msg><msg>Goodbye</msg></x>"
			val msgsStream = for {
				id <- XMLSplitter(* \ "id").first.asText.followedByStream
				msg <- XMLSplitter(* \ "msg").asText
			} yield s"$id:$msg"
			runTransformer(xml, msgsStream) should be(List("123:Hello", "123:Goodbye"))
		}
		it("should not pass a result until the followed parser has finished"){
			val xml = "<x><id>123</id><msg>Hello</msg><msg>Goodbye</msg></x>"
			// this parser does not give a result until the containing element ends,
			// so the "following" parser will never receive any events (besides the stack replay)
			val idsParser = XMLSplitter(* \ "id").asText.parseToList.map(_.mkString)
			val msgsParser = idsParser.followedByStream(id => XMLSplitter(* \ "msg").asText.map(msg => s"$id:$msg"))
			runTransformer(xml, msgsParser) should be(Nil)
		}
		it("should yield a *single* error if the followed parser yields an error"){
			val xml = "<x><id>ABC</id><msg>Hello</msg><msg>Goodbye</msg></x>"
			val idParser = XMLSplitter(* \ "id").first.asText.map(_.toInt) // will yield a Failure because of "ABC".toInt
			val msgsStream = idParser.followedByStream(id => XMLSplitter(* \ "msg").asText.map(msg => s"$id:$msg")).wrapSafe
			a[NumberFormatException] should be thrownBy {
				runTransformer(xml, msgsStream)
			}
		}
		it("should yield whatever errors the 'following' parser yields"){
			val xml = "<x><name>dylemma</name><num>1</num><num>B</num><num>3</num></x>"
			val nameParser = XMLSplitter(* \ "name").first.asText
			val numsStream = nameParser.followedByStream { name =>
				XMLSplitter(* \ "num").asText.map(s => s"$name:${s.toInt}").wrapSafe
			}
			runTransformer(xml, numsStream) should matchPattern {
				case Success("dylemma:1") :: Failure(_) :: Success("dylemma:3") :: Nil =>
			}
		}
	}

	describe("Parser.beforeContext"){
		it("should allow an optional parser to fail-fast before its .followedBy"){
			val xml =
				"""<root>
				  |  <data>1</data>
				  |  <data>2</data>
				  |</root>""".stripMargin
			val dataContext = * \ "data"
			def dataTransformer(prelude: Option[String]) = XMLSplitter(dataContext).asText.map(prelude -> _)
			val optPreludeParser = XMLSplitter(* \ "prelude").firstOption.attr("id") // will return None on the xml above
			val failFastPreludeParser = optPreludeParser.beforeContext(dataContext)

			// waiting until </root> to decide if the prelude parser returns None means the followedByStream sees nothing
			runTransformer(xml, optPreludeParser.followedByStream(dataTransformer)) should be(Nil)

			// sending an EOF to the prelude parser on the first <data> allows the followedByStream to receive the <data>s
			runTransformer(xml, failFastPreludeParser.followedByStream(dataTransformer)) should be {
				List(None -> "1", None -> "2")
			}
		}
	}
}