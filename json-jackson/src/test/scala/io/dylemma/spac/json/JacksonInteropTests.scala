package io.dylemma.spac.json

import io.dylemma.spac.json.JacksonSupport._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class JacksonInteropTests extends AnyFunSpec with Matchers with JsonParserBehaviors {
	describe("JSON support via Jackson") {
		it should behave like jsonParserWithStringSource
	}
}
