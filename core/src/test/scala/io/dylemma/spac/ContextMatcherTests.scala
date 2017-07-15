package io.dylemma.spac

import java.util
import javax.xml.namespace.QName
import javax.xml.stream.events.{Attribute, StartElement}

import org.scalamock.scalatest.MockFactory
import org.scalamock.util.Defaultable
import org.scalatest.{FunSpec, Matchers}

class ContextMatcherTests extends FunSpec with Matchers with MockFactory {

	// needed to mock some of the XMLEvent internals
	implicit val defaultIterator = new Defaultable[java.util.Iterator[_]] {
		override val default: util.Iterator[_] = null
	}

	def mockElem(name: String, attrs: (String, String)*): StartElement = {
		val elem = mock[StartElement]
		(elem.getName _).stubs().anyNumberOfTimes.returning(new QName(name))
		for((k, v) <- attrs){
			val attr = mock[Attribute]
			(attr.getValue _).stubs.returning(v)
			(elem.getAttributeByName _).stubs(new QName(k)).anyNumberOfTimes.returning(attr)
		}
		(elem.getAttributeByName _).stubs(where {
			qname: QName => !attrs.exists(_._1 == qname.getLocalPart)
		}).returning(null)
		elem
	}

	describe("single-element name matcher"){
		it("should succeed when the first element of the stack has the correct name"){
			val stack = Array(mockElem("hi"))
			val m = elem("hi")
			m(stack, 0, 1) should be('isDefined)
		}
		it("should fail when the first element of the stack is not the correct value"){
			val stack = Array(mockElem("bye"))
			val m = elem("hi")
			m(stack, 0, 1) should be('isEmpty)
		}
		it("should fail when the given length is 0"){
			val stack = Array(mockElem("hi"))
			val m = elem("hi")
			m(stack, 0, 0) should be('isEmpty)
		}
		it("should start matching from the specified offset"){
			val stack = Array("hi", "bye").map(mockElem(_))
			val m = elem("bye")
			m(stack, 0, 1) should be('isEmpty)
			m(stack, 1, 1) should be('isDefined)
		}
		it("should fail even if a matching element was further in the stack"){
			val stack = Array("x", "y").map(mockElem(_))
			val m = elem("y")
			m(stack, 0, 1) should be('isEmpty)
			m(stack, 1, 1) should be('isDefined)
		}
	}

	describe("single-element attr matcher"){
		it("should succeed when the first element of the stack has the correct attribute"){
			val stack = Array(mockElem("x", "foo" -> "bar"))
			val m = attr("foo")
			m(stack, 0, 1) should be('isDefined)
		}
		it("should return the value of the attribute when the attribute is present"){
			val stack = Array(mockElem("x", "foo" -> "bar"))
			val m = attr("foo")
			m(stack, 0, 1) should equal(Some("bar"))
		}
		it("should fail when the first element of the stack does not contain the correct attribute"){
			val stack = Array(mockElem("x"))
			val m = attr("foo")
			m(stack, 0, 1) should be('isEmpty)
		}
		it("should still function properly when created with a QName"){
			val stack = Array(mockElem("x", "foo" -> "bar"))
			val m1 = attr(new QName("foo"))
			val m2 = attr(new QName("y"))
			m1(stack, 0, 1) should equal(Some("bar"))
			m2(stack, 0, 1) should equal(None)
		}
		it("should fail if the stack is empty"){
			val m = attr("foo")
			m(IndexedSeq.empty, 0, 0) should be('isEmpty)
		}
	}

	describe("single-element `or` matcher"){
		it("should succeed when at least one of the combined matchers would succeed"){
			val m1 = elem("x")
			val m2 = elem("y")
			val m = m1 or m2
			m(Array(mockElem("x")), 0, 1) should be('isDefined)
			m(Array(mockElem("y")), 0, 1) should be('isDefined)
			m(Array(mockElem("z")), 0, 1) should be('isEmpty)
		}
		it("should return the match from the leftmost succeeding matcher"){
			val m = attr("x") | attr("y")
			m(Array(mockElem("_", "x" -> "1", "y" -> "2")), 0, 1) should equal(Some("1"))
			m(Array(mockElem("_", "y" -> "2")), 0, 1) should equal(Some("2"))
			m(Array(mockElem("_")), 0, 1) should equal(None)
		}
	}

	describe("single-element `and` matcher"){
		it("should succeed IFF both combined matchers succeed"){
			val m = elem("x") & attr("foo")
			m(Array(mockElem("x", "foo" -> "bar")), 0, 1) should be('isDefined)
			m(Array(mockElem("y", "foo" -> "bar")), 0, 1) should be('isEmpty)
			m(Array(mockElem("x")), 0, 1) should be('isEmpty)
		}
		it("should combine match results according to the TypeReduce rule: Unit + Unit = Unit"){
			val m = elem("x") & elem("x")
			m(Array(mockElem("x")), 0, 1) should equal(Some(()))
		}
		it("should combine match results according to the TypeReduce rule: T + Unit = T, and its converse, Unit + T = T"){
			val m1 = attr("foo") & elem("x")
			val m2 = elem("x") & attr("foo")
			val stack = Array(mockElem("x", "foo" -> "bar"))
			m1(stack, 0, 1) should equal(Some("bar"))
			m2(stack, 0, 1) should equal(Some("bar"))
		}
		it("should combine match results according to the TypeReduce rule: S + T = (S, T)"){
			val m = attr("x") & attr("y")
			val stack = Array(mockElem("_", "x" -> "1", "y" -> "2"))
			m(stack, 0, 1) should equal(Some("1" -> "2"))
		}
	}

	describe("chained matchers"){
		it("should apply a chain of two single-element matchers to the first two elements of the stack"){
			val m = elem("x") \ elem("y")
			val stack = Array("x", "y").map(mockElem(_))
			m(stack, 0, 2) should be('isDefined)
		}
		it("should not apply matchers to a later position in the stack"){
			val m = elem("x") \ elem("y")
			val stack = Array("w", "x", "y").map(mockElem(_))
			m(stack, 0, 3) should be('isEmpty)
		}
		it("should work with longer chains"){
			val m = elem("w") \ elem("x") \ elem("y") \ elem("z")
			val stack = Array("w", "x", "y", "z", "a", "b").map(mockElem(_))
			m(stack, 0, stack.length) should be('isDefined)
		}
		it("should combine extracted match values according to the TypeReduce rule: Unit + Unit = Unit"){
			val m = elem("x") \ elem("y")
			val stack = Array("x", "y").map(mockElem(_))
			m(stack, 0, 2) should equal(Some(()))
		}
		it("should combine extracted match values according to the TypeReduce rule: Unit + T = T"){
			val m = elem("x") \ attr("foo")
			val stack = Array(mockElem("x"), mockElem("y", "foo" -> "bar"))
			m(stack, 0, 2) should equal(Some("bar"))
		}
		it("should combine extracted match values according to the TypeReduce rule: S + T = (S, T)"){
			val m = attr("x") \ attr("y")
			val stack = Array(mockElem("_", "x" -> "1"), mockElem("_", "y" -> "2"))
			m(stack, 0, 2) should equal(Some("1" -> "2"))
		}
		it("should combine extracted match results such that S + T + U = (S + T) + U = ((S, T), U)"){
			val m = attr("x") \ attr("y") \ attr("z")
			val stack = Array(mockElem("_", "x" -> "1"), mockElem("_", "y" -> "2"), mockElem("_", "z" -> "3"))
			m(stack, 0, 3) should equal(Some((("1", "2"), "3")))
		}
		it("should be able to combine two chains"){
			val m1 = elem("x") \ elem("y")
			val m2 = elem("a") \ elem("b")
			val m = m1 \ m2
			val stack = Array("x", "y", "a", "b").map(mockElem(_))
			m(stack, 0, 4) should be('isDefined)
		}
	}

	describe("the `*` matcher"){
		// `*` is shadowed by imports from scalamock
		import ContextMatcherSyntax.{* => Star}

		it("should match any single element"){
			val elems = List(
				mockElem("x"),
				mockElem("y"),
				mockElem("_", "foo" -> "bar")
			)
			for(e <- elems){
				Star(Array(e), 0, 1) should be('isDefined)
			}
		}
		it("should fail on an empty stack"){
			Star(IndexedSeq.empty, 0, 0) should be('isEmpty)
		}
		it("should take up a single element when combined in a chain"){
			val m = elem("x") \ Star \ elem("y")
			val stack = Array("x", "_", "y").map(mockElem(_))
			m(stack, 0, stack.length) should be('isDefined)
		}
	}

	describe("the `**` matcher"){
		it("should consume any number of elements to make the next matcher succeed"){
			val m = ** \ elem("z")
			val stack = ('a' to 'z').map(c => mockElem(c.toString))
			m(stack, 0, stack.length) should be('isDefined)
		}
		it("should succeed even if the first greedy match would cause it to fail"){
			val m = ** \ elem("a") \ elem("b")
			val stack = Array("_", "_", "a", "a", "b").map(mockElem(_)) // the ** should consume [_, _, a]
			m(stack, 0, stack.length) should be('isDefined)
		}
		it("should work as expected while in the middle of a chain"){
			val m = elem("a") \ elem("b") \ ** \ elem("y") \ elem("z")
			val stack = ('a' to 'z').map(c => mockElem(c.toString))
			m(stack, 0, stack.length) should be('isDefined)
		}
		it("should succeed even if the entire stack doesn't need to be consumed to form the match"){
			val m = elem("a") \ ** \ elem("n")
			val stack = ('a' to 'z').map(c => mockElem(c.toString))
			m(stack, 0, stack.length) should be('isDefined)
		}
		it("should match the remainder of the stack if used at the end of the chain"){
			val m = elem("a") \ **
			val stack = ('a' to 'z').map(c => mockElem(c.toString))
			m(stack, 0, stack.length) should be('isDefined)
		}
	}
}
