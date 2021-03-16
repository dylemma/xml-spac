package io.dylemma.spac.xml

import java.io.{ByteArrayInputStream, Reader, StringReader}

import io.dylemma.spac.old.xml._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.io.{BufferedSource, Source}

class XMLResourceTests extends AnyFunSpec with Matchers {
	describe("XMLResource") {
		it("should be available for File") {
			"implicitly[XMLResource[java.io.File]]" should compile
		}
		it("should be available for String") {
			"implicitly[XMLResource[String]]" should compile
		}
		it("should be available for InputStream") {
			"implicitly[XMLResource[java.io.InputStream]]" should compile
		}
		it("should be available for Reader") {
			"implicitly[XMLResource[java.io.Reader]]" should compile
		}

		// contravariance checks
		it("should be available for InputStreamReader") {
			implicitly[XMLResource[java.io.InputStreamReader]] shouldEqual implicitly[XMLResource[java.io.Reader]]
		}
		it("should be available for BufferedInputStream") {
			implicitly[XMLResource[java.io.BufferedInputStream]] shouldEqual implicitly[XMLResource[java.io.InputStream]]
		}

		// for unsupported types, indirect support via functions that constructs a supported type
		it("should be available for resource-constructor-ish types") {
			val in = new ByteArrayInputStream("<foo><bar/></foo>".getBytes("UTF-8"))
			val bs: BufferedSource = Source.fromInputStream(in, "UTF-8")
			val parser = XMLParser.forText
			parser.parse(() => bs.reader) // rather than directly supporting BufferedSource, we support a `() => T` where `T: XMLResource`
			succeed // if the above line compiles and doesn't throw, we're all good here
		}

		/* We want to avoid resource leaks when using the `XMLResource[() => T]`.
		 * For normal `XMLResource[T]` if T is already a Closeable (e.g. InputStream or Reader),
		 * we assume that the responsibility for closing the stream/reader lies with whoever constructed
		 * it (which is not the XMLResource in those cases). But in the `() => T` case, the XMLResource
		 * *is* constructing the T, so the responsibility for closing it shifts.
		 * This test is to make sure the constructed resource's `close` method is called in that case.
		 */
		it("should attempt to close resources allocated by a constructor-style resource") {
			var attemptedClose = false
			val reader = new StringReader("<foo/>") {
				override def close(): Unit = {
					attemptedClose = true // the important bit
					super.close()
				}
			}
			val mockConstructor = () => reader
			val xr = implicitly[XMLResource[() => Reader]]
			val opened = xr.open(mockConstructor)
			xr.close(opened)
			assert(attemptedClose, "XMLResource[() => T] should close AutoCloseable objects that it creates")
		}

		it("should not attempt to close InputStreams that it did not construct") {
			var attemptedClose = false
			val in = new ByteArrayInputStream("<foo>hi</foo>".getBytes("UTF-8")) {
				override def close(): Unit = {
					println("But you closed the test InputStream???")
					new Exception("whoops").printStackTrace()
					attemptedClose = true
					super.close()
				}
			}
			XMLParser.forText.parse(in)
			attemptedClose should be(false)
		}

		it("should not attempt to close Readers that it did not construct") {
			var attemptedClose = false
			val reader = new StringReader("<foo>hi</foo>") {
				override def close(): Unit = {
					println("But you closed the test Reader???")
					new Exception("hey").printStackTrace()
					attemptedClose = true
					super.close()
				}
			}
			XMLParser.forText.parse(reader)
			attemptedClose should be(false)
		}
	}

}
