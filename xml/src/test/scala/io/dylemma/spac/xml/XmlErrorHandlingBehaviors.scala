package io.dylemma.spac
package xml

import _root_.fs2.Stream
import cats.effect.SyncIO
import cats.syntax.apply._
import io.dylemma.spac.interop.fs2._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

trait XmlErrorHandlingBehaviors { self: AnyFunSpec with Matchers =>

	/** Behavior suite that inspect the 'spac trace' of exceptions thrown in a handful of
	  * situations where the parse logic is somehow "wrong".
	  */
	def xmlErrorHandlingParserWithStringSource(
		stringToSource: String => Source[XmlEvent],
		stringToStream: String => Stream[SyncIO, XmlEvent],
	)(implicit support: ContextLineNumberSupport) = {

		case class ParserCase[A](parser: XmlParser[A], rawXml: String) {
			def checkBehaviorsWith(baseBehavior: (String, () => A) => Unit) = {
				describe(".parse") {
					it should behave like baseBehavior("parse", () => parser.parse(stringToSource(rawXml)))
				}
				describe(".toPipe") {
					def parseFromPipe() = parser.toPipe[SyncIO].apply(stringToStream(rawXml)).compile.lastOrError.unsafeRunSync()
					it should behave like baseBehavior("toPipe", parseFromPipe _)
				}
				describe(".parseF") {
					it should behave like baseBehavior("parseF", () => parser.parseF(stringToStream(rawXml)).unsafeRunSync())
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
			val RootLine = ContextLineMatcher(1)
			val ThingLine = ContextLineMatcher(2)
			val DataLine = ContextLineMatcher(3)

			val RootSplitterLine = new LineNumberCell
			val CompoundMemberLine = new LineNumberCell

			val barParser = XmlParser.attr("id")
			val fooParser = XmlParser.attrOpt("stuff")
			val dataParser = (
				XmlParser.attr("id"),
				Splitter.xml("data" \ "foo").joinBy(fooParser).parseToList,
				Splitter.xml("data" \ "bar").joinBy(barParser).parseFirstOpt,
			).tupled &: CompoundMemberLine
			val rootParser = RootSplitterLine & Splitter.xml("root" \ "thing" \ "data").joinBy(dataParser).parseToList

			ParserCase(rootParser, rawXml).checkBehaviorsWith { (methodName, doParse) =>
				it ("should include 'spac trace' information for the missing attribute") {
					intercept[XmlSpacException.MissingMandatoryAttributeException](doParse()).spacTrace.toList should matchPattern {
						case SpacTraceElement.InInput(StartElemNamed("data")) ::
							SpacTraceElement.InCompound(0, 3, CompoundMemberLine()) ::
							SpacTraceElement.InInputContext(StartElemNamed("data"), DataLine()) ::
							SpacTraceElement.InInputContext(StartElemNamed("thing"), ThingLine()) ::
							SpacTraceElement.InInputContext(StartElemNamed("root"), RootLine()) ::
							SpacTraceElement.InSplitter(_, RootSplitterLine()) :: // line number of rootParser in this test block
							SpacTraceElement.InParse("parser", `methodName`, _) ::
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
			val RootLine = ContextLineMatcher(1)
			val ThingLine = ContextLineMatcher(2)
			val DataLine = ContextLineMatcher(8)

			val BarSplitterLine = new LineNumberCell
			val RootSplitterLine = new LineNumberCell
			val CompoundLine = new LineNumberCell

			val barParser = XmlParser.attr("id")
			val fooParser = XmlParser.attrOpt("stuff")
			val dataParser = (
				XmlParser.attr("id"),
				Splitter.xml("data" \ "foo").joinBy(fooParser).parseToList,
				BarSplitterLine & Splitter.xml("data" \ "bar").joinBy(barParser).parseFirst,
			).tupled &: CompoundLine
			val rootParser = RootSplitterLine & Splitter.xml("root" \ "thing" \ "data").joinBy(dataParser).parseToList

			ParserCase(rootParser, rawXml).checkBehaviorsWith { (methodName, doParse) =>
				it ("should include 'spac trace' information for the missing item") {
					intercept[SpacException.MissingFirstException[_]](doParse()).spacTrace.toList should matchPattern {
						case SpacTraceElement.InInput(EndElemNamed("data")) ::
							SpacTraceElement.InSplitter(_, BarSplitterLine()) :: // splitter for the 3rd parser in dataParser
							SpacTraceElement.InCompound(2, 3, CompoundLine()) :: // index=2, meaning the 3rd parser
							SpacTraceElement.InInputContext(StartElemNamed("data"), DataLine()) ::
							SpacTraceElement.InInputContext(StartElemNamed("thing"), ThingLine()) ::
							SpacTraceElement.InInputContext(StartElemNamed("root"), RootLine()) ::
							SpacTraceElement.InSplitter(_, RootSplitterLine()) :: // the splitter for rootParser
							SpacTraceElement.InParse("parser", `methodName`, _) ::
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
			val RootLine = ContextLineMatcher(1)
			val ThingLine = ContextLineMatcher(2)
			val DataLine = ContextLineMatcher(3)

			val RootSplitterLine = new LineNumberCell
			val CompoundLine = new LineNumberCell

			val barParser = XmlParser.attr("id")
			val fooParser = XmlParser.attrOpt("stuff")
			val dataParser = (
				XmlParser.attr("id").map(_.toInt),
				Splitter.xml("data" \ "foo").joinBy(fooParser).parseToList,
				Splitter.xml("data" \ "bar").joinBy(barParser).parseFirstOpt,
			).tupled &: CompoundLine
			val rootParser = RootSplitterLine & Splitter.xml("root" \ "thing" \ "data").joinBy(dataParser).parseToList

			ParserCase(rootParser, rawXml).checkBehaviorsWith { (methodName, doParse) =>
				it("should capture the thrown exception and add 'spac trace' information") {
					val caughtError = intercept[SpacException.CaughtError] { doParse() }
					caughtError.nonSpacCause should matchPattern { case e: NumberFormatException => }
					caughtError.spacTrace.toList should matchPattern {
						case SpacTraceElement.InInput(StartElemNamed("data")) ::
							SpacTraceElement.InCompound(0, 3, CompoundLine()) :: // "id" parser is the 1st member of dataParser
							SpacTraceElement.InInputContext(StartElemNamed("data"), DataLine()) ::
							SpacTraceElement.InInputContext(StartElemNamed("thing"), ThingLine()) ::
							SpacTraceElement.InInputContext(StartElemNamed("root"), RootLine()) ::
							SpacTraceElement.InSplitter(_, RootSplitterLine()) ::
							SpacTraceElement.InParse("parser", `methodName`, _)
							:: Nil =>
						case 1 =>
					}
				}
			}
		}

		describe("running an exception-throwing transformer") {
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
			val RootLine = ContextLineMatcher(1)
			val ThingLine = ContextLineMatcher(2)
			val DataLine = ContextLineMatcher(3)

			val RootSplitterLine = new LineNumberCell
			val CompoundLine = new LineNumberCell

			val barParser = XmlParser.attr("id")
			val fooParser = XmlParser.attrOpt("stuff")
			val dataParser = (
				XmlParser.attr("id").map(_.toInt),
				Splitter.xml("data" \ "foo").joinBy(fooParser).parseToList,
				Splitter.xml("data" \ "bar").joinBy(barParser).parseFirstOpt,
			).tupled &: CompoundLine
			val rootTransformer = RootSplitterLine & Splitter.xml("root" \ "thing" \ "data").joinBy(dataParser)


			def expectedSpacTrace(parseMethod: String, ParseLine: LineNumberCell): PartialFunction[Any, _] = {
				case SpacTraceElement.InInput(StartElemNamed("data")) ::
					SpacTraceElement.InCompound(0, 3, CompoundLine()) ::
					SpacTraceElement.InInputContext(StartElemNamed("data"), DataLine()) ::
					SpacTraceElement.InInputContext(StartElemNamed("thing"), ThingLine()) ::
					SpacTraceElement.InInputContext(StartElemNamed("root"), RootLine()) ::
					SpacTraceElement.InSplitter(_, RootSplitterLine()) ::
					SpacTraceElement.InParse("transformer", `parseMethod`, ParseLine()) ::
					Nil => // ok
			}

			it ("as an iterator should provide 'spac trace' information") {
				val TransformLine = new LineNumberCell
				val outputItr = TransformLine & rootTransformer.transform {
					XmlParser.toList.parse(stringToSource(rawXml)).iterator
				}
				intercept[SpacException.CaughtError] { outputItr.toList }.spacTrace.toList should matchPattern(expectedSpacTrace("transform", TransformLine))
			}

			it ("as an fs2.Pipe should provide 'spac trace' information") {
				val ToPipeLine = new LineNumberCell
				val drainStreamIO = ToPipeLine & stringToStream(rawXml).through(rootTransformer.toPipe).compile.drain

				intercept[SpacException.CaughtError] { drainStreamIO.unsafeRunSync() }
					.spacTrace.toList
					.should(matchPattern(expectedSpacTrace("toPipe", ToPipeLine)))
			}
		}
	}
}
