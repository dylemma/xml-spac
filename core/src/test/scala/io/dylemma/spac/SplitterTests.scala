package io.dylemma.spac

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SplitterTests extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks {
	describe("Splitter.splitOnMatch") {
		val listsStartingWithOne = Splitter[Int].splitOnMatch(_ == 1).joinBy(Parser.toList) :> Parser.toList

		it("should create a new substream when an input matches the predicate") {
			listsStartingWithOne.parse(List(1, 2, 3, 1, 2, 1, 2, 3, 4, 5)) shouldEqual List(
				List(1, 2, 3),
				List(1, 2),
				List(1, 2, 3, 4, 5)
			)
		}

		it("should ignore prefix inputs if a substream hasn't started") {
			listsStartingWithOne.parse(List(5, 4, 3, 2, 1, 2, 3, 1, 2, 3)) shouldEqual List(
				List(1, 2, 3),
				List(1, 2, 3)
			)
		}

		it("should ignore all inputs if the predicate never matches") {
			listsStartingWithOne.parse(List(2, 3, 4, 5, 6, 7, 8, 9)) shouldEqual Nil
		}

		it("should handle an immediate EOF without starting any substreams") {
			listsStartingWithOne.parseSeq(List.empty[Int]) shouldEqual Nil
		}
	}

	describe("Splitter.consecutiveMatches") {
		val consecutiveAlphas = Splitter.consecutiveMatches[Char, Char] { case c if c.isLetter => c }
		val parseToString = Parser.fromBuilder { new StringBuilder }

		it("should create a substream for each sequence of consecutive matches") {
			consecutiveAlphas
				.joinBy(parseToString)
				.parseToList
				.parseIterator("123ABC456DEF789".iterator)
				.shouldEqual { List("ABC", "DEF") }
		}

		it("should use the first match in each substream as the context") {
			consecutiveAlphas
				.map { Parser.pure }
				.parseToList
				.parseIterator("123ABC456DEF789".iterator)
				.shouldEqual { List('A', 'D') }
		}
	}
}
