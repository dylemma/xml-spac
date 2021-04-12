package io.dylemma.spac
package xml

import cats.effect.SyncIO
import cats.syntax.apply._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

trait XmlErrorHandlingBehaviors { self: AnyFunSpec with Matchers =>

	/** Behavior suite that inspect the 'spac trace' of exceptions thrown in a handful of
	  * situations where the parse logic is somehow "wrong".
	  */
	def xmlParserWithStringSource(implicit stringParsable: Parsable[cats.Id, String, XmlEvent]) = {

		/** Since the tests in this file are sensitive to line numbers,
		  * this utility helps us capture the line number of whatever subject we're trying to test,
		  * so we can compare line numbers against a "cell" instead of a hard-coded int,
		  * and not worry about breaking tests when we add comments or imports
		  */
		class LineNumberCell {
			var valueOpt: Option[Int] = None
			def capture()(implicit pos: CallerPos) = {
				valueOpt = Some(pos.line)
				this
			}
			def get = valueOpt.getOrElse { fail("Expected to have a line number here") }
			def &[A](rhs: => A) = rhs

			def unapply(pos: CallerPos): Boolean = valueOpt match {
				case Some(line) => line == pos.line
				case _ => false
			}
		}

		case class ParserCase[A](parser: XmlParser[A], rawXml: String) {
			lazy val events = XmlParser.toList.parse(rawXml)
			def eventsItr = events.iterator

			def runParse(): A = parser.parse(rawXml)
			def runParseSeq(): A = parser.parse(events)
			def runParseIterator(): A = parser.parse(fs2.Stream.fromIterator[SyncIO](eventsItr, 1))

			def checkBehaviorsWith(baseBehavior: (String, () => A) => Unit) = {
				describe(".parse(rawSource)") {
					it should behave like baseBehavior("parse", runParse _)
				}
				describe(".parse(eventSequence)") {
					it should behave like baseBehavior("parse", runParseSeq _)
				}
				describe(".parse(eventStream)") {
					it should behave like baseBehavior("parse", runParseIterator _)
				}
			}
		}

		object StartElemNamed {
			def unapply(e: XmlEvent) = e match {
				case XmlEvent.ElemStart(se) => Some(se.name)
				case _ => None
			}
		}
		object EndElemNamed {
			def unapply(e: XmlEvent) = e match {
				case XmlEvent.ElemEnd(ee) => Some(ee.name)
				case _ => None
			}
		}
		object PosWithLine {
			def unapply(pos: CallerPos) = Some(pos.line)
		}

		object ContextWithLine {
			def unapply(loc: ContextLocation) = loc.get(ContextLineNumber)
		}

		describe("control case") {
			val rawXml =
				"""<root>
				  |   <thing>
				  |      <data id="hello">
				  |         <foo/>
				  |         <foo stuff="A" />
				  |         <bar id="B" />
				  |      </data>
				  |      <data id="123">
				  |         <foo stuff="C"/>
				  |      </data>
				  |   </thing>
				  |</root>""".stripMargin

			val barParser = XmlParser.attr("id")
			val fooParser = XmlParser.attrOpt("stuff")
			val dataParser = (
				XmlParser.attr("id"),
				Splitter.xml("data" \ "foo").joinBy(fooParser).parseToList,
				Splitter.xml("data" \ "bar").joinBy(barParser).parseFirstOpt,
			).tupled
			val rootParser = Splitter.xml("root" \ "thing" \ "data").joinBy(dataParser).parseToList

			ParserCase(rootParser, rawXml).checkBehaviorsWith { (methodName, doParse) =>
				it("should successfully parse the control input") {
					noException should be thrownBy doParse()
				}
			}
		}

		describe("missing 'id' attribute case") {
			val rawXml =
				"""<root>
				  |   <thing>
				  |      <data>
				  |         <foo/>
				  |         <foo stuff="A" />
				  |         <bar id="B" />
				  |      </data>
				  |      <data id="123">
				  |         <foo stuff="C"/>
				  |      </data>
				  |   </thing>
				  |</root>""".stripMargin

			val RootSplitterLine = new LineNumberCell

			val barParser = XmlParser.attr("id")
			val fooParser = XmlParser.attrOpt("stuff")
			val dataParser = (
				XmlParser.attr("id"),
				Splitter.xml("data" \ "foo").joinBy(fooParser).parseToList,
				Splitter.xml("data" \ "bar").joinBy(barParser).parseFirstOpt,
			).tupled
			val rootParser = RootSplitterLine.capture() & Splitter.xml("root" \ "thing" \ "data").joinBy(dataParser).parseToList

			ParserCase(rootParser, rawXml).checkBehaviorsWith { (methodName, doParse) =>
				it ("should include 'spac trace' information for the missing attribute") {
					intercept[XmlSpacException.MissingMandatoryAttributeException](doParse()).spacTrace.toList should matchPattern {
						case SpacTraceElement.InInput(StartElemNamed("data")) ::
							SpacTraceElement.InCompound(0, 3) ::
							SpacTraceElement.InInputContext(StartElemNamed("data"), ContextWithLine(3)) ::
							SpacTraceElement.InInputContext(StartElemNamed("thing"), ContextWithLine(2)) ::
							SpacTraceElement.InInputContext(StartElemNamed("root"), ContextWithLine(1)) ::
							SpacTraceElement.InSplitter(_, RootSplitterLine()) :: // line number of rootParser in this test block
							SpacTraceElement.InParse(`methodName`, _) ::
							Nil =>
					}
				}
			}
		}

		describe("missing first item inside splitter") {
			val rawXml =
				"""<root>
				  |   <thing>
				  |      <data id="hello">
				  |         <foo/>
				  |         <foo stuff="A" />
				  |         <bar id="B" />
				  |      </data>
				  |      <data id="123">
				  |         <foo stuff="C"/>
				  |      </data>
				  |   </thing>
				  |</root>""".stripMargin

			val BarSplitterLine = new LineNumberCell
			val RootSplitterLine = new LineNumberCell

			val barParser = XmlParser.attr("id")
			val fooParser = XmlParser.attrOpt("stuff")
			val dataParser = (
				XmlParser.attr("id"),
				Splitter.xml("data" \ "foo").joinBy(fooParser).parseToList,
				BarSplitterLine.capture() & Splitter.xml("data" \ "bar").joinBy(barParser).parseFirst,
			).tupled
			val rootParser = RootSplitterLine.capture() & Splitter.xml("root" \ "thing" \ "data").joinBy(dataParser).parseToList

			ParserCase(rootParser, rawXml).checkBehaviorsWith { (methodName, doParse) =>
				it ("should include 'spac trace' information for the missing item") {
					intercept[SpacException.MissingFirstException[_]](doParse()).spacTrace.toList should matchPattern {
						case SpacTraceElement.InInput(EndElemNamed("data")) ::
							SpacTraceElement.InSplitter(_, BarSplitterLine()) :: // splitter for the 3rd parser in dataParser
							SpacTraceElement.InCompound(2, 3) :: // index=2, meaning the 3rd parser
							SpacTraceElement.InInputContext(StartElemNamed("data"), _) ::
							SpacTraceElement.InInputContext(StartElemNamed("thing"), _) ::
							SpacTraceElement.InInputContext(StartElemNamed("root"), _) ::
							SpacTraceElement.InSplitter(_, RootSplitterLine()) :: // the splitter for rootParser
							SpacTraceElement.InParse(`methodName`, _) ::
							Nil =>
					}
				}
			}
		}

		describe("inner parser that throws errors") {
			val rawXml =
				"""<root>
				  |   <thing>
				  |      <data id="hello">
				  |         <foo/>
				  |         <foo stuff="A" />
				  |         <bar id="B" />
				  |      </data>
				  |      <data id="123">
				  |         <foo stuff="C"/>
				  |      </data>
				  |   </thing>
				  |</root>""".stripMargin

			val RootSplitterLine = new LineNumberCell

			val barParser = XmlParser.attr("id")
			val fooParser = XmlParser.attrOpt("stuff")
			val dataParser = (
				XmlParser.attr("id").map(_.toInt),
				Splitter.xml("data" \ "foo").joinBy(fooParser).parseToList,
				Splitter.xml("data" \ "bar").joinBy(barParser).parseFirstOpt,
			).tupled
			val rootParser = RootSplitterLine.capture() & Splitter.xml("root" \ "thing" \ "data").joinBy(dataParser).parseToList

			ParserCase(rootParser, rawXml).checkBehaviorsWith { (methodName, doParse) =>
				it("should capture the thrown exception and add 'spac trace' information") {
					val caughtError = intercept[SpacException.CaughtError](doParse())
					caughtError.nonSpacCause should matchPattern { case e: NumberFormatException => }
					caughtError.spacTrace.toList should matchPattern {
						case SpacTraceElement.InInput(StartElemNamed("data")) ::
							SpacTraceElement.InCompound(0, 3) :: // "id" parser is the 1st member of dataParser
							SpacTraceElement.InInputContext(StartElemNamed("data"), ContextWithLine(3)) ::
							SpacTraceElement.InInputContext(StartElemNamed("thing"), ContextWithLine(2)) ::
							SpacTraceElement.InInputContext(StartElemNamed("root"), ContextWithLine(1)) ::
							SpacTraceElement.InSplitter(_, RootSplitterLine()) ::
							SpacTraceElement.InParse(`methodName`, _)
							:: Nil =>
						case 1 =>
					}
				}
			}
		}

		describe("exception thrown from transformed iterator") {
			val rawXml =
				"""<root>
				  |   <thing>
				  |      <data id="hello">
				  |         <foo/>
				  |         <foo stuff="A" />
				  |         <bar id="B" />
				  |      </data>
				  |      <data id="123">
				  |         <foo stuff="C"/>
				  |      </data>
				  |   </thing>
				  |</root>""".stripMargin

			val RootSplitterLine = new LineNumberCell

			val barParser = XmlParser.attr("id")
			val fooParser = XmlParser.attrOpt("stuff")
			val dataParser = (
				XmlParser.attr("id").map(_.toInt),
				Splitter.xml("data" \ "foo").joinBy(fooParser).parseToList,
				Splitter.xml("data" \ "bar").joinBy(barParser).parseFirstOpt,
				).tupled
			val rootTransformer = RootSplitterLine.capture() & Splitter.xml("root" \ "thing" \ "data").joinBy(dataParser)
			val outputItr = rootTransformer.transform {
				XmlParser.toList.parse(rawXml).iterator
			}

			it ("should provide 'spac trace' information with the thrown exception") {
				intercept[SpacException.CaughtError] { outputItr.toList }.spacTrace.toList should matchPattern {
					case SpacTraceElement.InInput(StartElemNamed("data")) ::
						SpacTraceElement.InCompound(0, 3) ::
						SpacTraceElement.InInputContext(StartElemNamed("data"), ContextWithLine(3)) ::
						SpacTraceElement.InInputContext(StartElemNamed("thing"), ContextWithLine(2)) ::
						SpacTraceElement.InInputContext(StartElemNamed("root"), ContextWithLine(1)) ::
						SpacTraceElement.InSplitter(_, RootSplitterLine()) ::
						Nil => // doesn't end with `InParse` since there's no parser
				}
			}
		}
	}
}
