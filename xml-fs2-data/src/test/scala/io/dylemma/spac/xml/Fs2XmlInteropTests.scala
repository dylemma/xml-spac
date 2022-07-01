package io.dylemma.spac
package xml

import cats.effect.SyncIO
import io.dylemma.spac.ContextLineNumberSupport
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Fs2XmlInteropTests extends AnyFunSpec with Matchers with XmlParserBehaviors with XmlErrorHandlingBehaviors {
	describe("XML with fs2-data support") {
		def stringToSource(rawXml: String) = Fs2DataSource
			.fromString[SyncIO](rawXml)
			.compile
			.toList
			.map(Source.fromIterable(_))
			.unsafeRunSync()

		describe("Basic XML Parsing") {
			it should behave like xmlParserWithStringSource(stringToSource)
		}

		describe("Exception 'spac' trace handling (but with no line numbers for inputs)") {
			// fs2-data's XmlEvent model doesn't have a "location" concept, so we need to turn of the
			// line number matching for "InInputContext" elements in our spac-trace tests
			implicit val contextLineNumberSupport: ContextLineNumberSupport = ContextLineNumberSupport.Disabled

			it should behave like xmlErrorHandlingParserWithStringSource(stringToSource, Fs2DataSource.fromString[SyncIO](_))
		}
	}
}
