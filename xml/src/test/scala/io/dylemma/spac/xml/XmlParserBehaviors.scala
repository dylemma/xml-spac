package io.dylemma.spac
package xml

import cats.syntax.apply._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success, Try}

trait XmlParserBehaviors { this: AnyFunSpec with Matchers =>

	def xmlParserWithStringSource(stringToSource: String => Source[XmlEvent]) = {

		implicit class XmlStringContext(c: StringContext) {
			def xml(args: Any*) = stringToSource(c.s(args: _*))
		}

		describe("Parser.forText") {
			val textParser = XmlParser.forText

			it("should concatenate text events") {
				textParser
					.parse { xml"<foo>Hello<bar>World</bar><bar>Floopy</bar>Doop</foo>" }
					.shouldEqual { "HelloWorldFloopyDoop" }
			}

			it("should preserve whitespace") {
				textParser
					.parse { xml"<foo>\n\tHello\n</foo>" }
					.shouldEqual { "\n\tHello\n" }
			}
		}

		describe("Parser.forMandatoryAttribute") {
			val attrParser = XmlParser.forMandatoryAttribute("a")
			it("should return the attribute from the first encountered element") {
				attrParser
					.parse { xml"""<foo a="123"/>""" }
					.shouldEqual { "123" }
			}

			it("should return an error if the attribute is missing from the first encountered element") {
				an[XmlSpacException.MissingMandatoryAttributeException] should be thrownBy {
					attrParser
						.parse { xml"<foo/>" }
				}
			}

			it("should not return the attribute from an inner element") {
				an[XmlSpacException.MissingMandatoryAttributeException] should be thrownBy {
					attrParser
						.parse { xml"""<foo><bar a="123"/></foo>""" }
				}

				attrParser
					.parse { xml"""<foo a="123"><bar a="456"/></foo>""" }
					.shouldEqual { "123" }
			}
		}

		describe("Parser.forOptionalAttribute") {
			val attrOptParser = XmlParser.forOptionalAttribute("a")

			it("should return the attribute from the first encountered element, in a Some") {
				attrOptParser
					.parse { xml"""<foo a="123"/>""" }
					.shouldEqual { Some("123") }
			}

			it("should return None if the attribute is missing from the first encountered element") {
				attrOptParser
					.parse { xml"<foo/>" }
					.shouldEqual { None }
			}

			it("should not return the attribute from an inner element") {
				attrOptParser
					.parse { xml"""<foo><bar a="123"/></foo>""" }
					.shouldEqual { None }

				attrOptParser
					.parse { xml"""<foo a="123"><bar a="456"/></foo>""" }
					.shouldEqual { Some("123") }
			}
		}

		describe("Invalid XML") {
			it("should cause an exception to be thrown while running a parser") {
				intercept[Exception] {
					XmlParser.toList.parse {
						stringToSource("<doop oh hello this is just gibberish")
					}
				}
			}
		}

		describe("Parser#map") {
			it("should produce successful mapped results for valid inputs") {
				XmlParser.forText.map(_.toInt)
					.parse(xml"<foo>123</foo>")
					.shouldEqual(123)
			}

			it("should produce error results for invalid inputs") {
				an[Exception] should be thrownBy {
					XmlParser.forText.map(_.toInt) parse xml"<foo>ABC</foo>"
				}
			}
		}

		describe("XMLSplitter") {
			it("should filter out unmached events") {
				Splitter.xml(* \ "bar").text.parseToList
					.parse(xml"<foo><bar>Hello</bar><baz>World</baz></foo>")
					.shouldEqual { List("Hello") }
			}

			it("should split the events into substreams") {
				val rawXml = xml"<foo><bar>Hello</bar><bar>World</bar></foo>"
				val splitParser = Splitter.xml(* \ "bar").text.parseToList
				val unsplitParser = XmlParser.forText

				unsplitParser.parse(rawXml) shouldEqual "HelloWorld"
				splitParser.parse(rawXml) shouldEqual List("Hello", "World")
			}
		}

		describe("Parser.and") {
			it("should combine successful results of the combined parsers") {
				val rawXml = xml"""<foo a="123">hello world</foo>"""

				val preCombined = (
					XmlParser.forMandatoryAttribute("a"),
					XmlParser.forText
				)

				preCombined.tupled.parse(rawXml) shouldEqual { "123" -> "hello world" }
				preCombined.mapN(_ + _).parse(rawXml) shouldEqual "123hello world"

			}

			it("should result in an Error when any of the combined parsers does") {
				val parser = (XmlParser.forMandatoryAttribute("a").map(_.toInt), XmlParser.forText).tupled
				intercept[SpacException.CaughtError] {
					parser parse xml"""<foo a="abc">hello world</foo>"""
				} should matchPattern {
					case SpacException.CaughtError(e: NumberFormatException) =>
				}
			}

			it("should pass the same context value to its inner parsers if it requires a context") {
				class A
				case class AText(a: A, text: String)
				val splitter = Splitter.xml(attr("a").map(_ => new A))
				val rawXml = xml"""<foo a="123"><x>Hello</x><y>Goodbye</y></foo>"""

				// the two inner parsers should receive the same 'A' instance passed to this parser from a splitter
				def parseAText(context: A, elem: String) = {
					(Parser.pure(context), Splitter.xml(* \ elem).text.parseFirst).mapN(AText.apply)
				}

				val combinedContextualParser: XmlParser[(AText, AText)] = splitter.map { a =>
					(parseAText(a, "x"), parseAText(a, "y")).tupled
				}.parseFirst
				val (x, y) = splitter.joinBy(combinedContextualParser).parseFirst.parse(rawXml)
				x.a shouldEqual y.a
			}
		}

		describe("XMLSplitter.map") {
			val rawXml =
				xml"""<foo>
				     |	<a>1</a>
				     |	<a>2</a>
				     |	<b>3</b>
				     |	<a>4</a>
				     |	<b>5</b>
				     |</foo>"""
			val splitter = Splitter.xml("foo" \ extractElemName)

			sealed trait AB
			case class A(i: Int) extends AB
			case class B(i: Int) extends AB

			it("should choose the right sub-parser based on the context") {
				val abTransformer = splitter map {
					case "a" => XmlParser.forText.map(s => A(s.toInt))
					case "b" => XmlParser.forText.map(s => B(s.toInt))
				}

				abTransformer.parseToList.parse(rawXml) should matchPattern {
					case A(1) :: A(2) :: B(3) :: A(4) :: B(5) :: Nil =>
				}
			}
		}

		describe("Parser.followedBy") {
			it("should pass the result of the followed parser to create the resulting parser") {
				val xml = xml"<x><id>123</id><msg>Hello</msg><msg>Goodbye</msg></x>"
				val idParser = Splitter.xml(* \ "id").text.parseFirst
				val msgsParser = idParser.followedByParser { id =>
					Splitter.xml(* \ "msg").text.map(msg => s"$id:$msg").parseToList
				}
				msgsParser.parse(xml) shouldEqual List("123:Hello", "123:Goodbye")
			}
			it("should provide convenient flatMap syntax that works the same way") {
				val xml = xml"<x><id>123</id><msg>Hello</msg><msg>Goodbye</msg></x>"
				val msgsParser = for {
					id <- Splitter.xml(* \ "id").text.parseFirst.followedByParser
					msgs <- Splitter.xml(* \ "msg").text.map(msg => s"$id:$msg").parseToList
				} yield msgs
				msgsParser.parse(xml) shouldEqual List("123:Hello", "123:Goodbye")
			}
			it("should not pass a result until the followed parser has finished") {
				val xml = xml"<x><id>123</id><msg>Hello</msg><msg>Goodbye</msg></x>"
				// this parser does not give a result until the containing element ends,
				// so the "following" parser will never receive any events (besides the stack replay)
				val idsParser = Splitter.xml(* \ "id").text.parseToList.map(_.mkString)
				val msgsParser = idsParser.followedByParser(id => Splitter.xml(* \ "msg").text.map(msg => s"$id:$msg").parseToList)
				msgsParser.parse(xml) shouldEqual Nil
			}
			it("should yield an error if the followed parser yields an error") {
				val xml = xml"<x><id>ABC</id><msg>Hello</msg><msg>Goodbye</msg></x>"
				val idParser = Splitter.xml(* \ "id").text.parseFirst.map(_.toInt) // will yield a Failure because of "ABC".toInt
				val msgsParser = idParser.followedByParser(id => Splitter.xml(* \ "msg").text.map(msg => s"$id:$msg").parseToList)

				an[Exception] should be thrownBy {
					msgsParser parse xml
				}
			}

		}

		describe("Parser.followedByStream") {
			it("should pass the result of the followed parser to create the resulting transformer") {
				val xml = xml"<x><id>123</id><msg>Hello</msg><msg>Goodbye</msg></x>"
				val idParser = Splitter.xml(* \ "id").text.parseFirst
				val msgsStream = idParser.followedByStream { id => Splitter.xml(* \ "msg").text.map(msg => s"$id:$msg") }
				msgsStream.parseToList.parse(xml) shouldEqual List("123:Hello", "123:Goodbye")
			}
			it("should provide convenient flatMap syntax that works the same way") {
				val xml = xml"<x><id>123</id><msg>Hello</msg><msg>Goodbye</msg></x>"
				val msgsStream = for {
					id <- Splitter.xml(* \ "id").text.parseFirst.followedByStream
					msg <- Splitter.xml(* \ "msg").text
				} yield s"$id:$msg"
				msgsStream.parseToList.parse(xml) shouldEqual List("123:Hello", "123:Goodbye")
			}
			it("should not pass a result until the followed parser has finished") {
				val xml = xml"<x><id>123</id><msg>Hello</msg><msg>Goodbye</msg></x>"
				// this parser does not give a result until the containing element ends,
				// so the "following" parser will never receive any events (besides the stack replay)
				val idsParser = Splitter.xml(* \ "id").text.parseToList.map(_.mkString)
				val msgsParser = idsParser.followedByStream(id => Splitter.xml(* \ "msg").text.map(msg => s"$id:$msg"))
				msgsParser.parseToList.parse(xml) shouldEqual Nil
			}
			it("should yield a *single* error if the followed parser yields an error") {
				val xml = xml"<x><id>ABC</id><msg>Hello</msg><msg>Goodbye</msg></x>"
				val idParser = Splitter.xml(* \ "id").text.parseFirst.map(_.toInt) // will yield a Failure because of "ABC".toInt
				val msgsStream = idParser.followedByStream(id => Splitter.xml(* \ "msg").text.map(msg => s"$id:$msg"))
				intercept[SpacException.CaughtError] {
					msgsStream.parseToList.parse(xml)
				} should matchPattern {
					case SpacException.CaughtError(e: NumberFormatException) =>
				}
			}
			it("should yield whatever errors the 'following' parser yields") {
				val xml = xml"<x><name>dylemma</name><num>1</num><num>B</num><num>3</num></x>"
				val nameParser = Splitter.xml(* \ "name").text.parseFirst
				val numsStream = nameParser.followedByStream { name =>
					Splitter.xml(* \ "num").text.map(s => Try(s"$name:${ s.toInt }"))
				}
				numsStream.parseToList.parse(xml) should matchPattern {
					case Success("dylemma:1") :: Failure(_) :: Success("dylemma:3") :: Nil =>
				}
			}
		}

		describe("Parser.beforeContext") {
			it("should allow an optional parser to fail-fast before its .followedBy") {
				val xml =
					xml"""<root>
					     |  <data>1</data>
					     |  <data>2</data>
					     |</root>"""
				val dataContext = * \ "data"

				def dataTransformer(prelude: Option[String]) = Splitter.xml(dataContext).text.map(prelude -> _)

				val optPreludeParser = Splitter.xml(* \ "prelude").attr("id").parseFirstOpt // will return None on the xml above
				val failFastPreludeParser = optPreludeParser.beforeContext(dataContext)

				// waiting until </root> to decide if the prelude parser returns None means the followedByStream sees nothing
				optPreludeParser.followedByStream(dataTransformer).parseToList.parse(xml) shouldEqual Nil

				// sending an EOF to the prelude parser on the first <data> allows the followedByStream to receive the <data>s
				failFastPreludeParser.followedByStream(dataTransformer).parseToList.parse(xml) shouldEqual List(None -> "1", None -> "2")
			}
		}

		describe("Transformer.transform") {
			it("should work with arbitrary XML transforms from splitters") {
				val raw = xml"<a><b>hello</b><b>goodbye</b><b>so long...</b></a>"
				val eventsFromRaw = XmlParser.toList.parse(raw)
				val transformer = Splitter.xml("a" \ "b").text
				val itr = transformer.transform(eventsFromRaw.iterator)

				itr.hasNext should be(true)
				itr.next() should be("hello")

				itr.hasNext should be(true)
				itr.next() should be("goodbye")

				itr.hasNext should be(true)
				itr.next() should be("so long...")

				itr.hasNext should be(false)
			}
		}
	}
}
