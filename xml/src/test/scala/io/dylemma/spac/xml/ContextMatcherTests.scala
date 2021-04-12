package io.dylemma.spac.xml

import io.dylemma.spac.xml.TestXml.{testElem, testQName}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ContextMatcherTests extends AnyFunSpec with Matchers {

	private val _isDefined = Symbol("isDefined")
	private val _isEmpty = Symbol("isEmpty")

	describe("single-element name matcher"){
		it("should succeed when the first element of the stack has the correct name"){
			val stack = Array(testElem("hi"))
			val m = elem("hi")
			m(stack, 0, 1) should be(_isDefined)
		}
		it("should fail when the first element of the stack is not the correct value"){
			val stack = Array(testElem("bye"))
			val m = elem("hi")
			m(stack, 0, 1) should be(_isEmpty)
		}
		it("should fail when the given length is 0"){
			val stack = Array(testElem("hi"))
			val m = elem("hi")
			m(stack, 0, 0) should be(_isEmpty)
		}
		it("should start matching from the specified offset"){
			val stack = Array("hi", "bye").map(testElem(_))
			val m = elem("bye")
			m(stack, 0, 1) should be(_isEmpty)
			m(stack, 1, 1) should be(_isDefined)
		}
		it("should fail even if a matching element was further in the stack"){
			val stack = Array("x", "y").map(testElem(_))
			val m = elem("y")
			m(stack, 0, 1) should be(_isEmpty)
			m(stack, 1, 1) should be(_isDefined)
		}
	}

	describe("single-element attr matcher"){
		it("should succeed when the first element of the stack has the correct attribute"){
			val stack = Array(testElem("x", "foo" -> "bar"))
			val m = attr("foo")
			m(stack, 0, 1) should be(_isDefined)
		}
		it("should return the value of the attribute when the attribute is present"){
			val stack = Array(testElem("x", "foo" -> "bar"))
			val m = attr("foo")
			m(stack, 0, 1) should equal(Some("bar"))
		}
		it("should fail when the first element of the stack does not contain the correct attribute"){
			val stack = Array(testElem("x"))
			val m = attr("foo")
			m(stack, 0, 1) should be(_isEmpty)
		}
		it("should still function properly when created with a QName"){
			val stack = Array(testElem("x", "foo" -> "bar"))
			val m1 = attr(testQName("foo"))
			val m2 = attr(testQName("y"))
			m1(stack, 0, 1) should equal(Some("bar"))
			m2(stack, 0, 1) should equal(None)
		}
		it("should fail if the stack is empty"){
			val m = attr("foo")
			m(IndexedSeq.empty, 0, 0) should be(_isEmpty)
		}
	}

	describe("attrOpt matcher") {
		it("should succeed with None if the first element of the stack is missing the requested attribute") {
			val stack = Array(testElem("top"), testElem("next", "a" -> "1"))
			val m = attrOpt("a")
			m(stack, 0, 2) shouldEqual Some(None)
		}
		it("should capture the requested attribute from the first element in the stack") {
			val stack = Array(testElem("top", "a" -> "1"), testElem("next", "a" -> "2"))
			val m = attrOpt("a")
			m(stack, 0, 2) shouldEqual Some(Some("1"))
		}
		it("should fail if the stack is empty") {
			val m = attrOpt("a")
			m(IndexedSeq.empty, 0, 0) shouldEqual None
		}
		it("should combine with other single-element matchers") {
			val m = attrOpt("a") & attr("b")

			m(Array(testElem("top", "a" -> "1", "b" -> "hi")), 0, 1) shouldEqual Some(Some("1") -> "hi")
			m(Array(testElem("top", "a" -> "1")), 0, 1) shouldEqual None
		}
		it("should chain with other matchers") {
			val m = attrOpt("a") \ attrOpt("b")
			m(Array(testElem("top", "a" -> "1"), testElem("next", "b" -> "2")), 0, 2) shouldEqual Some(Some("1") -> Some("2"))
		}
	}

	describe("single-element `or` matcher"){
		it("should succeed when at least one of the combined matchers would succeed"){
			val m1 = elem("x")
			val m2 = elem("y")
			val m = m1 or m2
			m(Array(testElem("x")), 0, 1) should be(_isDefined)
			m(Array(testElem("y")), 0, 1) should be(_isDefined)
			m(Array(testElem("z")), 0, 1) should be(_isEmpty)
		}
		it("should return the match from the leftmost succeeding matcher"){
			val m = attr("x") | attr("y")
			m(Array(testElem("_", "x" -> "1", "y" -> "2")), 0, 1) should equal(Some("1"))
			m(Array(testElem("_", "y" -> "2")), 0, 1) should equal(Some("2"))
			m(Array(testElem("_")), 0, 1) should equal(None)
		}
	}

	describe("single-element `and` matcher"){
		it("should succeed IFF both combined matchers succeed"){
			val m = elem("x") & attr("foo")
			m(Array(testElem("x", "foo" -> "bar")), 0, 1) should be(_isDefined)
			m(Array(testElem("y", "foo" -> "bar")), 0, 1) should be(_isEmpty)
			m(Array(testElem("x")), 0, 1) should be(_isEmpty)
		}
		it("should combine match results according to the TypeReduce rule: Unit + Unit = Unit"){
			val m = elem("x") & elem("x")
			m(Array(testElem("x")), 0, 1) should equal(Some(()))
		}
		it("should combine match results according to the TypeReduce rule: T + Unit = T, and its converse, Unit + T = T"){
			val m1 = attr("foo") & elem("x")
			val m2 = elem("x") & attr("foo")
			val stack = Array(testElem("x", "foo" -> "bar"))
			m1(stack, 0, 1) should equal(Some("bar"))
			m2(stack, 0, 1) should equal(Some("bar"))
		}
		it("should combine match results according to the TypeReduce rule: S + T = (S, T)"){
			val m = attr("x") & attr("y")
			val stack = Array(testElem("_", "x" -> "1", "y" -> "2"))
			m(stack, 0, 1) should equal(Some("1" -> "2"))
		}
	}

	describe("chained matchers"){
		it("should apply a chain of two single-element matchers to the first two elements of the stack"){
			val m = elem("x") \ elem("y")
			val stack = Array("x", "y").map(testElem(_))
			m(stack, 0, 2) should be(_isDefined)
		}
		it("should not apply matchers to a later position in the stack"){
			val m = elem("x") \ elem("y")
			val stack = Array("w", "x", "y").map(testElem(_))
			m(stack, 0, 3) should be(_isEmpty)
		}
		it("should work with longer chains"){
			val m = elem("w") \ elem("x") \ elem("y") \ elem("z")
			val stack = Array("w", "x", "y", "z", "a", "b").map(testElem(_))
			m(stack, 0, stack.length) should be(_isDefined)
		}
		it("should combine extracted match values according to the TypeReduce rule: Unit + Unit = Unit"){
			val m = elem("x") \ elem("y")
			val stack = Array("x", "y").map(testElem(_))
			m(stack, 0, 2) should equal(Some(()))
		}
		it("should combine extracted match values according to the TypeReduce rule: Unit + T = T"){
			val m = elem("x") \ attr("foo")
			val stack = Array(testElem("x"), testElem("y", "foo" -> "bar"))
			m(stack, 0, 2) should equal(Some("bar"))
		}
		it("should combine extracted match values according to the TypeReduce rule: S + T = (S, T)"){
			val m = attr("x") \ attr("y")
			val stack = Array(testElem("_", "x" -> "1"), testElem("_", "y" -> "2"))
			m(stack, 0, 2) should equal(Some("1" -> "2"))
		}
		it("should combine extracted match results such that S + T + U = (S + T) + U = ((S, T), U)"){
			val m = attr("x") \ attr("y") \ attr("z")
			val stack = Array(testElem("_", "x" -> "1"), testElem("_", "y" -> "2"), testElem("_", "z" -> "3"))
			m(stack, 0, 3) should equal(Some((("1", "2"), "3")))
		}
		it("should be able to combine two chains"){
			val m1 = elem("x") \ elem("y")
			val m2 = elem("a") \ elem("b")
			val m = m1 \ m2
			val stack = Array("x", "y", "a", "b").map(testElem(_))
			m(stack, 0, 4) should be(_isDefined)
		}
	}

	describe("the `*` matcher"){

		it("should match any single element"){
			val elems = List(
				testElem("x"),
				testElem("y"),
				testElem("_", "foo" -> "bar")
			)
			for(e <- elems){
				*(Array(e), 0, 1) should be(_isDefined)
			}
		}
		it("should fail on an empty stack"){
			*(IndexedSeq.empty, 0, 0) should be(_isEmpty)
		}
		it("should take up a single element when combined in a chain"){
			val m = elem("x") \ * \ elem("y")
			val stack = Array("x", "_", "y").map(testElem(_))
			m(stack, 0, stack.length) should be(_isDefined)
		}
	}

	describe("the `**` matcher"){
		it("should consume any number of elements to make the next matcher succeed"){
			val m = ** \ elem("z")
			val stack = ('a' to 'z').map(c => testElem(c.toString))
			m(stack, 0, stack.length) should be(_isDefined)
		}
		it("should succeed even if the first greedy match would cause it to fail"){
			val m = ** \ elem("a") \ elem("b")
			val stack = Array("_", "_", "a", "a", "b").map(testElem(_)) // the ** should consume [_, _, a]
			m(stack, 0, stack.length) should be(_isDefined)
		}
		it("should work as expected while in the middle of a chain"){
			val m = elem("a") \ elem("b") \ ** \ elem("y") \ elem("z")
			val stack = ('a' to 'z').map(c => testElem(c.toString))
			m(stack, 0, stack.length) should be(_isDefined)
		}
		it("should succeed even if the entire stack doesn't need to be consumed to form the match"){
			val m = elem("a") \ ** \ elem("n")
			val stack = ('a' to 'z').map(c => testElem(c.toString))
			m(stack, 0, stack.length) should be(_isDefined)
		}
		it("should match the remainder of the stack if used at the end of the chain"){
			val m = elem("a") \ **
			val stack = ('a' to 'z').map(c => testElem(c.toString))
			m(stack, 0, stack.length) should be(_isDefined)
		}
	}
}
