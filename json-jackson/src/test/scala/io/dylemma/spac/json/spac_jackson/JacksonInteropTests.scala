package io.dylemma.spac
package json
package spac_jackson

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class JacksonInteropTests extends AnyFunSpec with Matchers with JsonParserBehaviors {
	describe("JSON support via Jackson") {
		it should behave like jsonParserWithStringSource
	}
}
