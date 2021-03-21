package io.dylemma.spac
package xml

import io.dylemma.spac.xml.TestXml._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class XmlTransformerTests extends AnyFunSpec with Matchers {
	describe("Transformer.transform"){
		it("should work with arbitrary XML transforms from splitters"){
			val raw = testXml"<a><b>hello</b><b>goodbye</b><b>so long...</b></a>"
			val transformer = Splitter.xml("a" \ "b").text
			val itr = transformer.transform(raw.iterator)

			itr.hasNext should be(true)
			itr.next() should be("hello")

			itr.hasNext should be(true)
			itr.next() should be("goodbye")

			itr.hasNext should be(true)
			itr.next() should be("so long...")

			itr.hasNext should be(false)
		}
	}
}
