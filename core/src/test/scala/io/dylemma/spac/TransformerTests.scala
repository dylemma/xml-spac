package io.dylemma.spac

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class TransformerTests extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks with SpacTestUtils {

	describe("Transformer # through") {
		it("should emit nothing when composing opposing filters") {
			val evens = Transformer[Int].filter(_ % 2 == 0)
			val odds = Transformer[Int].filter(_ % 2 == 1)
			val nothing = evens through odds
			forAll { (list: List[Int]) =>
				stepMany(nothing.newHandler, list).outputs shouldBe empty
			}
		}

		it("should pass outputs from 'this' as inputs to 'next'") {
			val addOne = Transformer[Int].map(_ + 1)
			val double = Transformer[Int].map(_ * 2)
			val combined = addOne through double
			forAll { (list: List[Int]) =>
				stepMany(combined.newHandler, list).outputs shouldEqual list.map(i => (i + 1) * 2)
			}
		}

		it("should respect an early-abort from either member") {
			val take3 = Transformer[Int].take(3)
			val drop3 = Transformer[Int].drop(3)

			val takeThenDrop = take3 through drop3
			val dropThenTake = drop3 through take3

			stepMany(takeThenDrop.newHandler, List(1, 2, 3, 4)) shouldEqual HandlerStep(Nil, Signal.Stop)

			forAll { (list: List[Int]) =>
				stepMany(dropThenTake.newHandler, list).outputs shouldEqual list.drop(3).take(3)
			}
		}
	}

	describe("Transformer # into") {
		it("should pipe the inner transformer's outputs to the given parser") {
			val t = Transformer[Int].map(_ * 2)
			val p = Parser[Int].toList
			forAll { (list: List[Int]) =>
				(t into p).parse(list) shouldEqual list.map(_ * 2)
			}
		}

		it("should exit early if the parser yields a result") {
			val p = Transformer[Int].map(_ * 2) into Parser.first
			forAll { (list: List[Int]) =>
				whenever(list.nonEmpty) {
					val expected = (list.head * 2, list.tail)
					p.newHandler.stepMany(list) shouldEqual { Left(expected) }
				}
			}
		}

		it("should finish the parser if the transformer finishes early") {
			val p = Transformer[Int].take(1) into Parser.toList
			forAll { (list: List[Int]) =>
				whenever(list.nonEmpty) {
					p.parse(list) shouldEqual List(list.head)
				}
			}
		}

		it("should finish the parser when the input ends") {
			val p = Transformer[Int].map(_ * 2) into Parser.toList
			forAll { (list: List[Int]) =>
				p.parse(list) shouldEqual list.map(_ * 2)
			}
		}
	}

	describe("Transformer # merge / mergeEither") {
		it("should emit outputs according to the structural order of the merge") {
			val t = (Transformer.identity[Int] mergeEither Transformer.identity[Int]).newHandler
			forAll { (i: Int) =>
				stepMany(t, i :: Nil).outputs shouldEqual List(Left(i), Right(i))
			}

			val t2 = (Transformer[Int].mapFlatten { i => List(i * 2, i * 3) } merge Transformer.identity[Int]).newHandler
			forAll { (i: Int) =>
				stepMany(t2, i :: Nil).outputs shouldEqual List(i * 2, i * 3, i)
			}
		}

		it ("should not end until both members end") {
			val earlyExit = Transformer[Int].take(1)
			val neverExit = Transformer.identity[Int]

			forAll { (list: List[Int]) =>
				whenever(list.lengthCompare(2) >= 0) {
					val t1 = earlyExit.merge(neverExit).newHandler
					val t2 = neverExit.merge(earlyExit).newHandler

					stepMany(t1, List(1,2,3)).signal shouldEqual Signal.Continue
					stepMany(t2, List(1,2,3)).signal shouldEqual Signal.Continue

					val t3 = earlyExit.merge(earlyExit).newHandler
					stepMany(t3, List(1,2,3)) shouldEqual HandlerStep(List(1, 1), Signal.Stop)
				}
			}
		}
	}

	describe("Transformer # scan") {
		it ("should emit the new scan state for every input") {
			val t = Transformer.identity[Int].scan(0)(_ + _).newHandler
			var sum = 0
			forAll { (num: Int) =>
				sum += num
				stepMany(t, num :: Nil) shouldEqual HandlerStep(sum :: Nil, Signal.Continue)
			}
		}
	}

	describe("Transformer # upcast") {
		it ("should not throw exceptions when casting") {
			val t = Transformer.identity[List[Int]]
			noException should be thrownBy {
				t.cast[Iterable[Int]]
				t.cast[Seq[Int]]
				t.cast[List[Int]]
			}
		}
	}

}
