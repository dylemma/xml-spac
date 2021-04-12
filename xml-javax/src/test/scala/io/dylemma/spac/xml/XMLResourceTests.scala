package io.dylemma.spac
package xml

import java.io.{ByteArrayInputStream, StringReader}

import cats.effect.{Resource, SyncIO}
import io.dylemma.spac.xml.spac_javax._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class XMLResourceTests extends AnyFunSpec with Matchers {
	describe("IntoXmlStreamReader") {
		it("should be available for File") {
			"implicitly[IntoXmlStreamReader[cats.effect.SyncIO, java.io.File]]" should compile
		}
		it("should be available for String") {
			"implicitly[IntoXmlStreamReader[cats.effect.SyncIO, String]]" should compile
		}
		it("should be available for Resource[F, InputStream]") {
			"implicitly[IntoXmlStreamReader[cats.effect.SyncIO, cats.effect.Resource[cats.effect.SyncIO, java.io.InputStream]]]" should compile
		}
		it("should be available for Resource[F, Reader]") {
			"implicitly[IntoXmlStreamReader[cats.effect.SyncIO,  cats.effect.Resource[cats.effect.SyncIO, java.io.Reader]]]" should compile
		}

		// contravariance checks
		it("should be available for InputStreamReader") {
			implicitly[IntoXmlStreamReader[SyncIO, Resource[SyncIO, java.io.InputStreamReader]]]
			implicitly[IntoXmlStreamReader[SyncIO, Resource[SyncIO, java.io.Reader]]]
		}
		it("should be available for BufferedInputStream") {
			implicitly[IntoXmlStreamReader[SyncIO, Resource[SyncIO, java.io.BufferedInputStream]]]
			implicitly[IntoXmlStreamReader[SyncIO, Resource[SyncIO, java.io.InputStream]]]
		}

		// for unsupported types, indirect support via functions that constructs a supported type
		it("should be available for resource-wrapped java.io source types") {
			// rather than directly supporting BufferedReader, we support a `Resource[F, BufferedReader]`
			val source = Resource.fromAutoCloseable(SyncIO {
				val in = new ByteArrayInputStream("<foo><bar/></foo>".getBytes("UTF-8"))
				Source.fromInputStream(in, "UTF-8").reader()
			})
			val parser = XmlParser.forText
			parser.parse(source) shouldEqual ""
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
		describe("Resource based sources") {

			it("should defer to the resource's close logic") {
				var attemptedClose = false
				val reader = new StringReader("<foo/>") {
					override def close(): Unit = {
						attemptedClose = true // the important bit
						super.close()
					}
				}
				val readerResource = Resource.fromAutoCloseable(SyncIO { reader })
				assert(!attemptedClose, "Reader should not be closed yet")
				XmlParser.pure(42).parse(readerResource)
				assert(attemptedClose, "Reader should have been closed when the resource was released")
			}

			it("should not attempt to close the underlying source if the resource does not (for InputStream sources)") {
				var attemptedClose = false
				val in = new ByteArrayInputStream("<foo>hi</foo>".getBytes("UTF-8")) {
					override def close(): Unit = {
						println("But you closed the test InputStream???")
						new Exception("whoops").printStackTrace()
						attemptedClose = true
						super.close()
					}
				}
				val nonClosingResource = Resource.eval(SyncIO.pure(in))
				assert(!attemptedClose, "Stream should not be closed initially")
				XmlParser.forText.parse(nonClosingResource) shouldEqual "hi"
				assert(!attemptedClose, "Stream should not be closed after being `.parse`d")
			}

			it("should not attempt to close the underlying source if the resource does not (for Reader sources)") {
				var attemptedClose = false
				val reader = new StringReader("<foo>hi</foo>") {
					override def close(): Unit = {
						println("But you closed the test Reader???")
						new Exception("hey").printStackTrace()
						attemptedClose = true
						super.close()
					}
				}
				val nonClosingSource = Resource.eval(SyncIO.pure(reader))
				XmlParser.forText.parse(nonClosingSource)
				attemptedClose should be(false)
			}
		}
	}

}
