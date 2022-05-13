package io.dylemma.spac.json

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayInputStream, StringReader}

class JacksonSourceTests extends AnyFunSpec with Matchers {
	describe("JacksonSource") {
		describe(".fromInputStream") {
			it("should not attempt to close the stream") {
				var attemptedClose = false
				val in = new ByteArrayInputStream("[1,2,3]".getBytes("UTF-8")) {
					override def close(): Unit = {
						println("But you closed the test InputStream???")
						new Exception("whoops").printStackTrace()
						attemptedClose = true
						super.close()
					}
				}
				val src = JacksonSource.fromInputStream(in)

				assert(!attemptedClose, "Stream should not be closed initially")
				JsonParser.listOf[Int].parse(src)
				assert(!attemptedClose, "Stream should not be closed after being `.parse`d")
			}
		}

		describe(".fromReader") {
			it("should not attempt to close the reader") {
				var attemptedClose = false
				val reader = new StringReader("true") {
					override def close(): Unit = {
						attemptedClose = true // the important bit
						super.close()
					}
				}
				val src = JacksonSource.fromReader(reader)

				assert(!attemptedClose, "Reader should not be closed initially")
				JsonParser.forBoolean.parse(src)
				assert(!attemptedClose, "Reader should not be closed after being `.parse`d")
			}
		}
	}
}