package io.dylemma.spac

import cats.effect.SyncIO
import io.dylemma.spac.impl.ParserOrElseList
import io.dylemma.spac.impl.ParserOrElseList.NoSuccessfulParsersException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ParserTests extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks with SpacTestUtils {

	describe("Parser # map") {
		val firstOptParser = Parser[SyncIO, Int].firstOpt
		val listParser = Parser[SyncIO, Int].toList

		it("should return a result equivalent to running the mapping function on the base parser's result") {
			def invariant[A, B](base: Parser[SyncIO, Int, A], f: A => B) = {
				forAll { (list: List[Int]) =>
					base.map(f).parse(list).unsafeRunSync() shouldEqual f(base.parse(list).unsafeRunSync())
				}
			}

			invariant[Option[Int], String](firstOptParser, _.fold("")("It was " + _))
			invariant[List[Int], String](listParser, _.mkString("_"))
		}
		it("should pull the same number of inputs from the source as the base parser would have") {
			def invariant[A, B](base: Parser[SyncIO, Int, A], f: A => B) = {
				forAll { (list: List[Int]) =>
					val basePullCount = countPullsIn(list) { base.parse(_).unsafeRunSync() }
					val mappedPullCount = countPullsIn(list) { base.map(f).parse(_).unsafeRunSync() }
					basePullCount shouldEqual mappedPullCount
				}
			}

			invariant[Option[Int], String](firstOptParser, _.fold("")("It was " + _))
			invariant[List[Int], String](listParser, _.mkString("_"))
		}
		describe("with a mapping function that throws exceptions") {
			val dummyException = new Exception("oh no")
			val errorProneParser = listParser.map[String](_ => throw dummyException)
			it("should capture exceptions thrown by the mapping function and lift them into the effect context") {
				forAll { (list: List[Int]) =>
					errorProneParser.parse(list).attempt.unsafeRunSync() shouldEqual Left(dummyException)
				}
			}
			it("should not throw the exception until the base parser has yielded a result") {
				forAll { (list: List[Int]) =>
					countPullsIn(list) { errorProneParser.parse(_).attempt.unsafeRunSync() } shouldEqual (list.size + 1)
				}
			}
		}
	}

	describe("Parser # orElse") {
		val p1 = Parser[SyncIO, Int].firstOpt.map(_ => "firstOpt")
		val p2 = Parser[SyncIO, Int].toList.map(_ => "toList")
		val p3 = Parser[SyncIO, Int].fold("")(_ + _)
		val err1 = new Exception("Error 1")
		val err2 = new Exception("Error 2")
		val pErr1 = Parser.eval { SyncIO.raiseError(err1) }
		val pErr2 = Parser.eval { SyncIO.raiseError(err2) }

		it ("should return the result from the earliest underlying parser that succeeds") {
			forAll { (list: List[Int]) =>
				whenever (list.nonEmpty) {
					p1.orElse(p2).parse(list).unsafeRunSync() shouldEqual "firstOpt"
					p2.orElse(p1).parse(list).unsafeRunSync() shouldEqual "firstOpt"
				}
			}
		}
		it ("should return the result whichever successful parser was earliest in the orElse chain") {
			forAll { (list: List[Int]) =>
				p2.orElse(p3).parse(list).unsafeRunSync() shouldEqual "toList"
				p3.orElse(p2).parse(list).unsafeRunSync() shouldEqual list.mkString
			}
		}
		it ("should discard errors from underlying parsers as long as at least one succeeds") {
			forAll { (list: List[Int]) =>
				p2.orElse(pErr1).parse(list).unsafeRunSync() shouldEqual "toList"
				pErr1.orElse(p2).parse(list).unsafeRunSync() shouldEqual "toList"
			}
		}
		it ("should raise a NoSuccessfulParsersException if all of the underlying parsers fail") {
			forAll { (list: List[Int]) =>
				val e = intercept[NoSuccessfulParsersException]{
					pErr1.orElse(pErr2).parse(list).unsafeRunSync()
				}
				e.getSuppressed.toList shouldEqual List(err2, err1)
			}
		}
		it ("should allow many parsers to be composed together into a single fallback chain") {
			val parser = p2 orElse p3 orElse pErr1 orElse pErr2 orElse p1
			forAll { (list: List[Int]) =>
				// normally p1 will finish first because it finishes on the first step,
				// but if the list is empty, everything finishes at once, so the tiebreaker is which one is earlier in the chain
				parser.parse(list).unsafeRunSync() shouldEqual (if(list.isEmpty) "toList" else "firstOpt")
			}
			// making sure that repeatedly calling `orElse` will build up one big chain rather than a hierarchy of chains
			parser should matchPattern {
				// Scalac is being stupid and doesn't understand this line
				// case ParserOrElseList(Right(`p2`) :: Right(`p3`) :: Right(`pErr1`) :: Right(`pErr2`) :: Right(`p1`) :: Nil) =>
				// so I'm doing it this way instead >:(
				case ParserOrElseList(Right(a) :: Right(b) :: Right(c) :: Right(d) :: Right(e) :: Nil)
					if (a == p2) && (b == p3) && (c == pErr1) && (d == pErr2) && (e == p1) =>
			}
		}
	}
}
