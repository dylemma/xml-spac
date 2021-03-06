package io.dylemma.spac
package xml

import io.dylemma.spac.xml.JavaxSupport._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class JavaxInteropTests extends AnyFunSpec with Matchers with XmlParserBehaviors with XmlErrorHandlingBehaviors {
	describe("XML with Javax support") {
		describe("Basic XML Parsing") {
			it should behave like xmlParserWithStringSource
		}

		describe("Exception 'spac' trace handling") {
			implicit val contextLineNumberSupport: ContextLineNumberSupport = ContextLineNumberSupport.Enabled
			it should behave like xmlErrorHandlingParserWithStringSource(JavaxSource.syncIO(_))
		}
	}
}
