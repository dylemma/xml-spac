package io.dylemma.spac
package xml

import spac_javax._

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class JavaxInteropTests extends AnyFunSpec with Matchers with XmlErrorHandlingBehaviors {
	describe("XML with Javax support") {
		describe("Exception 'spac' trace handling") {
			it should behave like xmlParserWithStringSource
		}
	}
}
