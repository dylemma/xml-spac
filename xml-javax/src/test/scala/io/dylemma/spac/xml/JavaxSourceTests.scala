package io.dylemma.spac
package xml

import _root_.fs2.Stream
import cats.effect.SyncIO
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayInputStream, StringReader}

class JavaxSourceTests extends AnyFunSpec with Matchers with XmlParserBehaviors with XmlErrorHandlingBehaviors {

	describe("JavaxSource") {

		describe("Basic XML Parsing") {
			it should behave like xmlParserWithStringSource(JavaxSource.fromString(_))
		}

		describe("Exception 'spac' trace handling") {
			implicit val contextLineNumberSupport: ContextLineNumberSupport = ContextLineNumberSupport.Enabled
			it should behave like xmlErrorHandlingParserWithStringSource(
				JavaxSource.fromString(_),
				s => Stream(JavaxSource.fromString(s).iterateWith(_.toList): _*).covary[SyncIO],
			)
		}

		/* The gist here is that the library should generally not close an auto-closeable source
		 * unless it was the one responsible for constructing that source in the first place.
		 * We represent this at a type level by only supporting java.io stream-like sources if
		 * they are wrapped in a `cats.effect.Resource`, so that the caller can explicitly control
		 * how the close logic gets applied.
		 *
		 * The general guidance is:
		 *  - If you want spac to open+close the stream, use `Resource.fromAutoCloseable(SyncIO { constructTheStream })`
		 *  - If you want spac to NOT close the stream, use `Resource.liftF(SyncIO { existingStream })`
		 */
		describe("resource closing behavior") {

			describe(".fromReader") {
				var attemptedClose = false
				val reader = new StringReader("<foo/>") {
					override def close(): Unit = {
						attemptedClose = true // the important bit
						super.close()
					}
				}
				val source = JavaxSource.fromReader(reader)

				it("should not close the underlying stream") {
					assert(!attemptedClose, "Reader should not be closed yet")
					XmlParser.pure(42).parse(source)
					assert(!attemptedClose, "Reader should not have been closed after being consumed")
				}
				it("should throw upon attempted re-iteration") {
					assertThrows[IllegalStateException] {
						source.iterateWith(_ => ())
					}
				}
			}

			describe(".fromInputStream") {
				var attemptedClose = false
				val in = new ByteArrayInputStream("<foo>hi</foo>".getBytes("UTF-8")) {
					override def close(): Unit = {
						attemptedClose = true
						super.close()
					}
				}
				val source = JavaxSource.fromInputStream(in)
				it("should not close the underlying stream") {
					assert(!attemptedClose, "Stream should not be closed initially")
					XmlParser.forText.parse(source) shouldEqual "hi"
					assert(!attemptedClose, "Stream should not be closed after being `.parse`d")
				}
				it("should throw upon attempted re-iteration") {
					assertThrows[IllegalStateException] {
						source.iterateWith(_ => ())
					}
				}
			}
		}
	}
}
