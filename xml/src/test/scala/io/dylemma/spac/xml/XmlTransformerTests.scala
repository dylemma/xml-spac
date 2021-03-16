package io.dylemma.spac.xml

import io.dylemma.spac.old.xml._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class XmlTransformerTests extends AnyFunSpec with Matchers {
	describe("Transformer.transform"){
		it("should work with arbitrary XML transforms from splitters"){
			val raw = "<a><b>hello</b><b>goodbye</b><b>so long...</b></a>"
			val transformer = XMLSplitter("a" \ "b").asText
			val itr = transformer.transform(raw)

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
