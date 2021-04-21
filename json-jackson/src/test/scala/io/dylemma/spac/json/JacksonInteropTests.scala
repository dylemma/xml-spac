package io.dylemma.spac.json

import io.dylemma.spac.json.JacksonSupport._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class JacksonInteropTests extends AnyFunSpec with Matchers with JsonParserBehaviors with JsonErrorHandlingBehaviors {
	describe("JSON support via Jackson") {
		describe("base behavior") {
			it should behave like jsonParserWithStringSource
		}
		describe("Exception 'spac' trace handling") {
			it should behave like jsonErrorHandlingParserWithStringSource(JacksonSource.syncIO(_))
		}
	}

}
