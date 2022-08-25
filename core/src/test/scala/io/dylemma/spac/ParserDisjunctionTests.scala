package io.dylemma.spac

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ParserDisjunctionTests extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks with SpacTestUtils {

	describe("Parser # or") {
		it ("should create a disjunction that succeeds if either parser succeeds") {
			val evenChecker = Parser[Int].first.map(_ % 2 == 0)
			val oddChecker = Parser[Int].first.map(_ % 2 == 1)
			val eitherChecker = evenChecker.or(oddChecker)

			eitherChecker.parse(Source.fromIterable(List(2))) shouldBe true
			eitherChecker.parse(Source.fromIterable(List(1))) shouldBe true
		}

		it ("should combine parsers associatively") {
			val A = Parser[Int].first.map(_ % 4 == 0)
			val B = Parser[Int].first.map(_ % 4 == 1)
			val C = Parser[Int].first.map(_ % 4 == 2)
			val D = Parser[Int].first.map(_ % 4 == 3)

			(A || B || C || D) shouldEqual ((A || B) || (C || D))
			((A || B) || C || D) shouldEqual (A || (B || C) || D)
			(A || B || C || D) shouldEqual (A or B or C or D)
		}

		it ("should eagerly return true if an inner parser returns true") {
			val awaitEven = Transformer[Int].dropWhile(_ % 2 != 0).parseFirst.map(_ => true)
			val awaitMultipleOfThree = Transformer[Int].dropWhile(_ % 3 != 0).parseFirst.map(_ => true)
			val inputs = List(1, 5, 7, 10, 11, 9)
			countPullsIn(inputs) { counted =>
				(awaitEven || awaitMultipleOfThree).parse(counted.iterator)
			} shouldEqual 4
		}

		it ("should finish any unfinished parsers at the end of the input") {
			val A = Parser[Int].first.map(_ => false)
			val B = Transformer[Int].filter(_ => false).parseFirstOpt.map(_ => true)
			val C = Transformer[Int].filter(_ => false).parseFirstOpt.map(_ => false)
			countPullsIn(0 until 10) { counted =>
				(A || B).parse(counted.iterator) shouldBe true
			} shouldEqual 10
			countPullsIn(0 until 10) { counted =>
				(A || C).parse(counted.iterator) shouldBe false
			}
		}
	}
}
