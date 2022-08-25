package io.dylemma.spac

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ParserConjunctionTests extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks with SpacTestUtils {

	describe("Parser # and") {
		it ("should create a disjunction that fails if either parser fails") {
			val evenChecker = Parser[Int].first.map(_ % 2 == 0)
			val oddChecker = Parser[Int].first.map(_ % 2 == 1)
			val eitherChecker = evenChecker.and(oddChecker)
			eitherChecker.parse(Source.fromIterable(List(2))) shouldBe false
			eitherChecker.parse(Source.fromIterable(List(1))) shouldBe false
		}

		it ("should combine parsers associatively") {
			val A = Parser[Int].first.map(_ % 4 == 0)
			val B = Parser[Int].first.map(_ % 4 == 1)
			val C = Parser[Int].first.map(_ % 4 == 2)
			val D = Parser[Int].first.map(_ % 4 == 3)

			(A && B && C && D) shouldEqual ((A && B) && (C && D))
			((A && B) && C && D) shouldEqual (A && (B && C) && D)
			(A && B && C && D) shouldEqual (A and B and C and D)
		}

		it ("should eagerly return false if an inner parser returns false") {
			val expectNoOdds = Transformer[Int].filter(_ % 2 == 1).parseFirstOpt.map {
				case None => true
				case Some(odd) => false
			}
			val expectNoMultiplesOfThree = Transformer[Int].filter(_ % 3 == 0).parseFirstOpt.map {
				case None => true
				case Some(multiple) => false
			}
			val both = expectNoOdds && expectNoMultiplesOfThree
			val inputs = List(2, 4, 6, 8, 9)
			// `both` should fail upon encountering the `6` (3rd input) due to `expectNoMultiplesOfThree`
			countPullsIn(inputs) { counted => both.parse(counted.iterator) } shouldEqual 3
		}

		it ("should return true at the end if none of the inner parsers return by then") {
			val A = Parser[Int].first.map(_ => true)
			val B = Transformer[Int].filter(_ => false).parseFirstOpt.map(_ => false)
			val C = Transformer[Int].filter(_ => false).parseFirstOpt.map(_ => true)
			countPullsIn(0 until 10) { counted =>
				(A && B).parse(counted.iterator) shouldBe false
			} shouldEqual 10
			countPullsIn(0 until 10) { counted =>
				(A && C).parse(counted.iterator) shouldBe true
			}
		}
	}
}
