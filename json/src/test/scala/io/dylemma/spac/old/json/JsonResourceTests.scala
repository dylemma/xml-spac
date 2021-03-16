package io.dylemma.spac.old.json

import java.io.{ByteArrayInputStream, StringReader}

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.io.{BufferedSource, Source}

class JsonResourceTests extends AnyFunSpec with Matchers {
	describe("JsonResource") {
		it("should be available for File") {
			"implicitly[JsonResource[java.io.File]]" should compile
		}
		it("should be available for String") {
			"implicitly[JsonResource[String]]" should compile
		}
		it("should be available for InputStream") {
			"implicitly[JsonResource[java.io.InputStream]]" should compile
		}
		it("should be available for Reader") {
			"implicitly[JsonResource[java.io.Reader]]" should compile
		}

		// contravariance checks
		it("should be available for InputStreamReader") {
			implicitly[JsonResource[java.io.InputStreamReader]] shouldEqual implicitly[JsonResource[java.io.Reader]]
		}
		it("should be available for BufferedInputStream") {
			implicitly[JsonResource[java.io.BufferedInputStream]] shouldEqual implicitly[JsonResource[java.io.InputStream]]
		}

		// for unsupported types, indirect support via functions that constructs a supported type
		it("should be available for resource-constructor-ish types") {
			val source: BufferedSource = Source.fromInputStream(new ByteArrayInputStream("[1,2,3]".getBytes("UTF-8")), "UTF-8")
			val parser = JsonParser.listOf[Int]
			parser.parse(() => source.reader) // rather than directly supporting BufferedSource, we support a `() => T` where `T: XMLResource`
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
			val constructReader = () => new StringReader("true") {
				override def close(): Unit = {
					attemptedClose = true // the important bit
					super.close()
				}
			}
			JsonParser.forBoolean.parse(constructReader)
			assert(attemptedClose, "JsonResource[() => T] should close AutoCloseable objects that it creates")
		}

		it("should not attempt to close InputStreams that it did not construct") {
			var attemptedClose = false
			val in = new ByteArrayInputStream("[1,2,3]".getBytes("UTF-8")) {
				override def close(): Unit = {
					println("But you closed the test InputStream???")
					new Exception("whoops").printStackTrace()
					attemptedClose = true
					super.close()
				}
			}
			JsonParser.listOf[Int].parse(in)
			attemptedClose should be(false)
		}

		it("should not attempt to close Readers that it did not construct") {
			var attemptedClose = false
			val reader = new StringReader("[1,2,3]") {
				override def close(): Unit = {
					println("But you closed the test Reader???")
					new Exception("hey").printStackTrace()
					attemptedClose = true
					super.close()
				}
			}
			JsonParser.listOf[Int].parse(reader)
			attemptedClose should be(false)
		}
	}
}
