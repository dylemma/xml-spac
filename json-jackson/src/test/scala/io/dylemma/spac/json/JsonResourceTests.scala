package io.dylemma.spac.json

import java.io.{ByteArrayInputStream, StringReader}

import cats.effect.{Resource, SyncIO}
import io.dylemma.spac.json.JacksonSupport._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class JsonResourceTests extends AnyFunSpec with Matchers {
	describe("JsonResource") {
		it("should be available for File") {
			"implicitly[IntoJacksonJsonParser[cats.effect.SyncIO, java.io.File]]" should compile
		}
		it("should be available for String") {
			"implicitly[IntoJacksonJsonParser[cats.effect.SyncIO, String]]" should compile
		}
		it("should be available for Resource[F, InputStream]") {
			"implicitly[IntoJacksonJsonParser[cats.effect.SyncIO, cats.effect.Resource[cats.effect.SyncIO, java.io.InputStream]]]" should compile
		}
		it("should be available for Resource[F, Reader]") {
			"implicitly[IntoJacksonJsonParser[cats.effect.SyncIO,  cats.effect.Resource[cats.effect.SyncIO, java.io.Reader]]]" should compile
		}

		// contravariance checks
		it("should be available for InputStreamReader") {
			implicitly[IntoJacksonJsonParser[SyncIO, Resource[SyncIO, java.io.InputStreamReader]]]
			implicitly[IntoJacksonJsonParser[SyncIO, Resource[SyncIO, java.io.Reader]]]
		}
		it("should be available for BufferedInputStream") {
			implicitly[IntoJacksonJsonParser[SyncIO, Resource[SyncIO, java.io.BufferedInputStream]]]
			implicitly[IntoJacksonJsonParser[SyncIO, Resource[SyncIO, java.io.InputStream]]]
		}

		// for unsupported types, indirect support via functions that constructs a supported type
		it("should be available for resource-wrapped java.io source types") {
			val source = Resource.fromAutoCloseable(SyncIO {
				val in = new ByteArrayInputStream("[1,2,3]".getBytes("UTF-8"))
				Source.fromInputStream(in, "UTF-8").reader()
			})
			val parser = JsonParser.listOf[Int]
			parser.parse(source) // rather than directly supporting BufferedSource, we support a `() => T` where `T: XMLResource`
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
			val reader = new StringReader("true") {
				override def close(): Unit = {
					attemptedClose = true // the important bit
					super.close()
				}
			}
			val readerResource = Resource.fromAutoCloseable(SyncIO.pure(reader))
			JsonParser.forBoolean.parse(readerResource)
			assert(attemptedClose, "Reader should have been closed when the resource was released")
		}

		it("should not attempt to close the underlying source if the resource does not (for InputStream sources)") {
			var attemptedClose = false
			val in = new ByteArrayInputStream("[1,2,3]".getBytes("UTF-8")) {
				override def close(): Unit = {
					println("But you closed the test InputStream???")
					new Exception("whoops").printStackTrace()
					attemptedClose = true
					super.close()
				}
			}
			val nonClosingResource = Resource.eval(SyncIO.pure(in))
			assert(!attemptedClose, "Stream should not be closed initially")
			JsonParser.listOf[Int].parse(nonClosingResource)
			assert(!attemptedClose, "Stream should not be closed after being `.parse`d")
		}

		it("should not attempt to close the underlying source if the resource does not (for Reader sources)") {
			var attemptedClose = false
			val reader = new StringReader("[1,2,3]") {
				override def close(): Unit = {
					println("But you closed the test Reader???")
					new Exception("hey").printStackTrace()
					attemptedClose = true
					super.close()
				}
			}
			val nonClosingSource = Resource.eval(SyncIO.pure(reader))
			JsonParser.listOf[Int].parse(nonClosingSource)
			attemptedClose should be(false)
		}
	}
}