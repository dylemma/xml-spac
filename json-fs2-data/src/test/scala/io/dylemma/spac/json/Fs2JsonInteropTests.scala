package io.dylemma.spac
package json

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Fs2JsonInteropTests extends AnyFunSpec with Matchers with JsonParserBehaviors with JsonErrorHandlingBehaviors {
	describe("JSON with fs2-data support") {
		import Fs2DataSupport._
		describe("Basic JSON Parsing") {
			it should behave like jsonParserWithStringSource
		}

		describe("Exception 'spac' trace handling (but with no line numbers for inputs)") {
			// fs2-data's Token model doesn't have a "location" concept, so we need to turn of the
			// line number matching for "InInputContext" elements in our spac-trace tests
			implicit val contextLineNumberSupport: ContextLineNumberSupport = ContextLineNumberSupport.Disabled
			it should behave like jsonErrorHandlingParserWithStringSource(Fs2DataSource.syncIO(_))
		}
	}
}
