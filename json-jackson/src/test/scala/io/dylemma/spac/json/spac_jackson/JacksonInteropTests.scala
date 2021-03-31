package io.dylemma.spac
package json
package spac_jackson

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class JacksonInteropTests extends AnyFunSpec with Matchers with io.dylemma.spac.json.JsonParserBehaviors {
	describe("JSON support via Jackson") {
		it should behave like jsonParserWithStringSource
	}
}
