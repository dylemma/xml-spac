package io.dylemma.spac
package xml

import io.dylemma.spac.xml.JavaxSupport._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class JavaxInteropTests extends AnyFunSpec with Matchers with XmlErrorHandlingBehaviors {
	describe("XML with Javax support") {
		describe("Exception 'spac' trace handling") {
			implicit val contextLineNumberSupport: ContextLineNumberSupport = ContextLineNumberSupport.Enabled
			it should behave like xmlParserWithStringSource(JavaxSource.syncIO(_))
		}
	}
}
