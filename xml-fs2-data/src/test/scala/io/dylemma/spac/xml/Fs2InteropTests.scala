package io.dylemma.spac.xml

import io.dylemma.spac.ContextLineNumberSupport
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import Fs2DataSupport._

class Fs2InteropTests extends AnyFunSpec with Matchers with XmlParserBehaviors with XmlErrorHandlingBehaviors {
	describe("XML with fs2-data support") {
		describe("Basic XML Parsing") {
			it should behave like xmlParserWithStringSource
		}

		describe("Exception 'spac' trace handling (but with no line numbers for inputs)") {
			// fs2-data's XmlEvent model doesn't have a "location" concept, so we need to turn of the
			// line number matching for "InInputContext" elements in our spac-trace tests
			implicit val contextLineNumberSupport: ContextLineNumberSupport = ContextLineNumberSupport.Disabled

			it should behave like xmlErrorHandlingParserWithStringSource(Fs2DataSource.syncIO(_))
		}
	}
}
