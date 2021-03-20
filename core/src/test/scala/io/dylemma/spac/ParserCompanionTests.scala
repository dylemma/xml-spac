package io.dylemma.spac

import cats.data.Chain
import cats.effect.SyncIO
import cats.implicits._
import cats.{Applicative, Traverse}
import io.dylemma.spac.impl.ParserCompoundN
import org.scalacheck.Arbitrary
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.collection.mutable
import scala.reflect.ClassTag

class ParserCompanionTests extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks with SpacTestUtils {

	describe("Parser.firstOpt") {
		def firstOptParser[In: Arbitrary](parser: Parser[In, Option[In]]) = {
			it("should return the first element of the source stream") {
				forAll { (list: List[In]) =>
					parser.parseSeq(list) shouldEqual list.headOption
				}
			}
			it("should return None if the source stream is empty") {
				parser.parseSeq(Nil) shouldEqual None
			}
			it("`.parseSeq` should consume exactly one element from the source sequence") {
				countPullsIn[In] { parser.parseSeq(_) } shouldEqual 1
			}
			it("`.parse` should consume exactly one element from the source stream") {
				countPullsIn[In] { parser.parse(_) } shouldEqual 1
			}
		}

		describe("Parser.firstOpt[A]") {
			it should behave like firstOptParser(Parser.firstOpt[Int])
		}
		describe("Parser[A].firstOpt") {
			it should behave like firstOptParser(Parser[Int].firstOpt)
		}
	}

	describe("Parser.first") {
		def firstParser[In: Arbitrary : ClassTag](parser: Parser[In, In]) = {
			it("should return the first element of a non-empty source") {
				forAll { (list: List[In]) =>
					whenever(list.nonEmpty) {
						parser.parseSeq(list) shouldEqual list.head
					}
				}
			}
			it("should throw a MissingFirstException when parsing an empty sequence") {
				assertThrows[SpacException.MissingFirstException[In]] {
					parser.parseSeq(Nil)
				}
			}
			it("`.parseSeq` should consume exactly one element from the source sequence") {
				countPullsIn[In] { parser.parseSeq(_) } shouldEqual 1
			}
			it("`.parse` should consume exactly one element from the source stream") {
				countPullsIn[In] { parser.parse(_) } shouldEqual 1
			}
		}

		describe("Parser.first[In]") {
			it should behave like firstParser(Parser.first[Int])
		}
		describe("Parser[In].first") {
			it should behave like firstParser(Parser[Int].first)
		}
	}

	describe("Parser.find") {
		val dummyException = new Exception("oh no")

		def findParser(makeParser: (Int => Boolean) => Parser[Int, Option[Int]]) = {
			val parser = makeParser(_ % 2 == 0)
			val errorProneParser = makeParser {
				case 0 => throw dummyException
				case 1 => true
				case _ => false
			}

			it("should return None when parsing an empty list") {
				parser.parseSeq(Nil) shouldEqual None
			}
			it("should return Some when parsing a list which contains a matching value") {
				parser.parseSeq(List(2)) shouldEqual Some(2)
			}
			it("should return None when parsing a list which does not contain any matching value") {
				parser.parseSeq(List(1, 3, 5, 7, 9)) shouldEqual None
			}
			describe(".parseSeq") {
				it("should not pull values past the point where a matching value is found") {
					countPullsIn(List(1, 2, 3, 4)) { parser.parseSeq(_) } shouldEqual 2
				}
				it("should pull the entire input if no matching value is found") {
					countPullsIn(List(1, 3, 5, 7)) { parser.parseSeq(_) } shouldEqual 5
				}
			}
			describe(".parse") {
				it("should not pull values past the point where a matching value is found") {
					countPullsIn(List(1, 2, 3, 4)) { parser.parse(_) } shouldEqual 2
				}
				it("should pull the entire input if no matching value is found") {
					countPullsIn(List(1, 3, 5, 7)) { parser.parse(_) } shouldEqual 5
				}
			}
			describe("with an exception-throwing predicate") {
				it("should capture thrown exceptions in the effect context") {
					intercept[Exception] { errorProneParser.parse(List(0)) } shouldEqual dummyException
				}
				it("should stop pulling values from the source when an exception is thrown") {
					countPullsIn(List(3, 2, 0, 1)) { s => intercept[Exception] { errorProneParser.parse(s) } } shouldEqual 3
				}
			}
		}

		describe("Parser.find[In]") {
			it should behave like findParser(Parser.find[Int])
		}
		describe("Parser[In].find") {
			it should behave like findParser(Parser[Int].find)
		}
	}

	describe("Parser.fold") {
		val dummyException = new Exception("oh no, fold failure")

		def foldParser(makeParser: String => ((String, Int) => String) => Parser[Int, String]) = {
			val parser = makeParser("") { (accum, next) => accum + next }
			val errorProneParser = makeParser("") {
				case (accum, 0) => throw dummyException
				case (accum, i) => accum + i
			}

			it("should return the initial value when parsing an empty list") {
				parser.parseSeq(Nil) shouldEqual ""
			}
			it("should aggregate the inputs in the order they are encountered") {
				parser.parseSeq(List(4, 2, 0, 6, 9)) shouldEqual "42069"
			}
			describe("with an exception-throwing aggregator function") {
				it("should capture thrown exceptions in the effect context") {
					intercept[Exception] { errorProneParser.parseSeq(List(3, 2, 1, 0)) } shouldEqual dummyException
				}
				it("should stop consuming inputs after an exception is thrown") {
					countPullsIn(List(3, 2, 1, 0, 1, 2, 3)) { s =>
						intercept[Exception] { errorProneParser.parseSeq(s) }
					} shouldEqual 4
				}
			}
			it("`.parse` should pull all inputs") {
				countPullsIn(List(1, 2, 3, 4)) { parser.parse(_) } shouldEqual 5
				countPullsIn[List, Int](Nil) { parser.parse(_) } shouldEqual 1
			}
			it("`.parseSeq` should pull all inputs") {
				countPullsIn(List(1, 2, 3, 4)) { parser.parseSeq(_) } shouldEqual 5
				countPullsIn[List, Int](Nil) { parser.parseSeq(_) } shouldEqual 1
			}
		}

		describe("Parser.fold[In, Out]") {
			it should behave like foldParser(Parser.fold[Int, String])
		}
		describe("Parser[In].fold[Out]") {
			it should behave like foldParser(Parser[Int].fold[String])
		}
	}

	describe("Parser.pure") {
		def pureParser(makeParser: Int => Parser[Any, Int]) = {
			val parser = makeParser(42)
			it("should return the value and ignore the inputs from the source") {
				forAll { (list: List[Int]) =>
					parser.parseSeq(list) shouldEqual 42
				}
			}
			it("should return the wrapped value even if the source is empty") {
				parser.parseSeq(Nil) shouldEqual 42
			}
			it("`.parseSeq` should pull only input") {
				countPullsIn[Int] { parser.parseSeq(_) } shouldEqual 1
			}
			it("`.parse` should pull only 1 input") {
				countPullsIn[Int] { parser.parse(_) } shouldEqual 1
			}
		}

		describe("Parser.pure[Out]") {
			it should behave like pureParser(Parser.pure[Int])
		}
		describe("Parser[In].pure[Out]") {
			it should behave like pureParser(Parser[Any].pure[Int])
		}
	}

	describe("Parser.defer ") {
		def deferParser(makeParser: (=> Parser[Int, String]) => Parser[Int, String]) = {
			def innerParser1 = Parser.pure("yay")

			def innerParser2 = Parser[Int].fold("")(_ + _)

			val dummyException = new Exception("oh no")

			def throwDummy = throw dummyException // convincing intellij that defining `parser3` doesn't cause "unreachable code"

			val parser1 = makeParser { innerParser1 }
			val parser2 = makeParser { innerParser2 }
			val parser3 = makeParser { throwDummy }

			it("should return the same value that the eval-returned parser would") {
				forAll { (list: List[Int]) =>
					parser1.parseSeq(list) shouldEqual innerParser1.parseSeq(list)
					parser2.parseSeq(list) shouldEqual innerParser2.parseSeq(list)
				}
			}

			it("should only pull as many values from the source as the eval-returned parser would") {
				countPullsIn(List(1, 2, 3)) { parser1.parseSeq(_) } shouldEqual 1
				countPullsIn(List(1, 2, 3)) { parser2.parseSeq(_) } shouldEqual 4
			}

			it("should bubble up exceptions thrown by the parser constructor when asked create a handler") {
				intercept[Exception] { parser3.newHandler } shouldEqual dummyException
			}
		}

		describe("Parser.defer[In, Out]") {
			it should behave like deferParser(Parser.defer[Int, String])
		}
		describe("Parser[In].defer[Out]") {
			it should behave like deferParser(Parser[Int].defer[String])
		}
	}

	describe("Parser.delay") {
		def delayParser(makeParser: (=> String) => Parser[Int, String]) = {
			val dummyException = new Exception("oh no")

			def throwDummy = throw dummyException // convincing intellij that defining `parser3` doesn't cause "unreachable code"

			val parser1 = makeParser { "yay" }
			val parser2 = makeParser { throwDummy }

			it("should not call the construction function until a constructed handler is asked to step or finish") {
				locally {
					var didConstruct = false
					val parser1 = makeParser {
						didConstruct = true
						"yay"
					}
					didConstruct shouldEqual false
					parser1.newHandler.step(1) shouldEqual Left("yay")
					didConstruct shouldEqual true
				}
				locally {
					var didConstruct = false
					val parser1 = makeParser {
						didConstruct = true
						"yay"
					}
					didConstruct shouldEqual false
					parser1.newHandler.finish() shouldEqual "yay"
					didConstruct shouldEqual true
				}
			}

			it("should bubble up exceptions thrown by the parser constructor when asked to step or finish") {
				val parser = makeParser { throwDummy }
				intercept[Exception] { parser.newHandler.step(1) } shouldEqual dummyException
				intercept[Exception] { parser.newHandler.finish() } shouldEqual dummyException
			}
		}

		describe("Parser.delay[Out]") {
			it should behave like delayParser(Parser.delay[String])
		}
		describe("Parser[In].delay[Out]") {
			it should behave like delayParser(Parser[Int].delay[String])
		}
	}

	describe("Parser.toChain / Parser.toList") {
		def _collectorBehavior[C[_], A: Arbitrary](
			collectionName: String,
			parser: Parser[A, C[A]],
			materializeCollection: List[A] => C[A],
		) = {
			it(s"should collect all inputs from the source into a $collectionName") {
				forAll { (list: List[A]) =>
					parser.parseSeq(list) shouldEqual materializeCollection(list)
				}
			}
			it("should always pull all inputs from the source") {
				forAll { (list: List[A]) =>
					countPullsIn(list) { parser.parseSeq(_) } shouldEqual (list.size + 1)
				}
			}
			it("should be reusable") {
				forAll { (list: List[A]) =>
					parser.parseSeq(list) shouldEqual parser.parseSeq(list)
				}
			}
		}

		def toListParser(parser: Parser[Int, List[Int]]) = {
			_collectorBehavior[List, Int]("List", parser, identity)
		}

		def toChainParser(parser: Parser[Int, Chain[Int]]) = {
			_collectorBehavior[Chain, Int]("Chain", parser, Chain.fromSeq)
		}

		describe("Parser.toList[In]") {
			it should behave like toListParser(Parser.toList[Int])
		}
		describe("Parser[In].toList") {
			it should behave like toListParser(Parser[Int].toList)
		}
		describe("Parser.toChain[In]") {
			it should behave like toChainParser(Parser.toChain[Int])
		}
		describe("Parser[In].toChain") {
			it should behave like toChainParser(Parser[Int].toChain)
		}
	}

	describe("Parser.fromBuilder") {
		def fromBuilderParser(makeParser: (=> mutable.Builder[Int, List[Int]]) => Parser[Int, List[Int]]) = {
			it("should add each input from the source to the builder, returning the result at the end of the stream") {
				val parser = makeParser { List.newBuilder }
				forAll { (list: List[Int]) =>
					parser.parseSeq(list) shouldEqual list
				}
			}
			it("should construct a new builder when it is asked to parse a source, to avoid sharing builders") {
				// effectively the same test as above, but with a different motivation
				val parser = makeParser { List.newBuilder }
				val result1 = parser.parseSeq(List(1, 2, 3))
				val result2 = parser.parseSeq(List(1, 2, 3))
				result1 shouldEqual result2
				result1 shouldEqual List(1, 2, 3)
			}
			describe("when constructed with a shared builder reference") {
				it("re-running the parser will continue to accumulate values in the builder") {
					// this is what you SHOULD NOT do when using Parser.impureBuild
					val sharedBuilder = List.newBuilder[Int]
					val sharingParser = makeParser(sharedBuilder)
					val result1 = sharingParser.parseSeq(List(1, 2, 3))
					val result2 = sharingParser.parseSeq(List(4, 5, 6))
					result1 shouldEqual List(1, 2, 3)
					result2 shouldEqual List(1, 2, 3, 4, 5, 6) // i.e. be careful not to share builders!
				}
			}
		}

		describe("Parser.fromBuilder[In, Out]") {
			it should behave like fromBuilderParser(Parser.fromBuilder[Int, List[Int]])
		}
		describe("Parser[In].impureBuild[Out]") {
			it should behave like fromBuilderParser(Parser[Int].fromBuilder[List[Int]])
		}
	}

	describe("Applicative[Parser]") {
		val F = Applicative[Parser[Int, *]]

		val p1 = Parser[Int].toList.withName("P1")
		val p2 = Parser[Int].firstOpt.withName("P2")
		val p3 = Parser[Int].fold("")(_ + _).withName("P3")
		val p4 = Parser[Int].pure("hello").withName("P4")

		val dummyException = new Exception("oh no")
		val dummyException2 = new Exception("oh yeah!")
		val pErr = Parser[Int].delay { throw dummyException }
		val pConditionalError = Parser[Int].fold("") {
			case (_, 0) => throw dummyException2
			case (accum, next) => accum + next
		}

		def inspectCompound[F[+_], In, Out](parser: Parser[In, Out]) = parser match {
			case compound: ParserCompoundN[In, Out] => Some(compound.members)
			case _ => None
		}

		describe(".product") {
			it("should result in a ParserCompoundN with two underlying parsers") {
				val pN = F.product(p1, p2)
				inspectCompound(pN) shouldEqual Some {
					Chain(p1, p2)
				}
			}
			it("should combine the output from the underlying parsers") {
				val pN = F.product(p1, p2)
				pN.parseSeq(List(1, 2, 3)) shouldEqual {
					List(1, 2, 3) -> Some(1)
				}
			}
			it("should only pull each event from the source once") {
				val pN = F.product(p1, p3)
				forAll { (list: List[Int]) =>
					countPullsIn(list) { pN.parseSeq(_) } shouldEqual (list.size + 1)
				}
			}
			it("should catch and raise errors from the underlying parsers") {
				val pN = F.product(p2, pErr)
				forAll { (list: List[Int]) =>
					intercept[Exception] { pN.parse(list) } shouldEqual dummyException
				}
			}
			it("should abort execution upon encountering an error from an underlying parser") {
				countPullsIn[Int] { F.product(p2, pErr).wrapSafe.parseSeq(_) } shouldEqual 1
				forAll { (list: List[Int]) =>
					val indexOf0 = list.indexOf(0)
					val numPullsExpected = if (indexOf0 < 0) list.size + 1 else indexOf0 + 1
					val pN = F.product(p1, pConditionalError)
					countPullsIn(list) { pN.wrapSafe.parseSeq(_) } shouldEqual numPullsExpected
				}
			}
		}

		describe("SemigroupalOps.tupled") {
			def compound(arg: (Parser[Int, Any], List[Parser[Int, Any]])) = {
				val (p, parsers) = arg
				it("should create a ParserCompositeN with underlying parsers in a similar order") {
					val expected = Chain.fromSeq(parsers)
					inspectCompound(p) shouldEqual Some(expected)
				}

				it("should yield the same product of results as if the underlying parsers were run separately and then combined") {
					forAll { (list: List[Int]) =>
						val compoundResults = p
							.attempt
							.parseSeq(list)
							.asInstanceOf[Either[Throwable, Product]]
							.map(tryFlattenTuple)
						val expectedResults = Traverse[List]
							.sequence(parsers.map(_.attempt.parseSeq(list)))
						compoundResults shouldEqual expectedResults
					}
				}
			}

			describe("(p1, p2)") {
				it should behave like compound { (p1, p2).tupled -> List(p1, p2) }
			}
			describe("(pErr, p3)") {
				it should behave like compound { (pErr, p3).tupled -> List(pErr, p3) }
			}

			describe("(p1, p2, p3)") {
				it should behave like compound { (p1, p2, p3).tupled -> List(p1, p2, p3) }
			}
			describe("((p1, p2), p3)") {
				it should behave like compound { ((p1, p2).tupled, p3).tupled -> List(p1, p2, p3) }
			}
			describe("(p1, (p2, p3))") {
				it should behave like compound { (p1, (p2, p3).tupled).tupled -> List(p1, p2, p3) }
			}

			describe("(p1, p2, p3, p4)") {
				it should behave like compound { (p1, p2, p3, p4).tupled -> List(p1, p2, p3, p4) }
			}
			describe("(p1, (p2, p3, p4))") {
				it should behave like compound { (p1, (p2, p3, p4).tupled).tupled -> List(p1, p2, p3, p4) }
			}
			describe("((p1, p2, p3), p4)") {
				it should behave like compound { ((p1, p2, p3).tupled, p4).tupled -> List(p1, p2, p3, p4) }
			}
			describe("((p1, p2), (p3, p4))") {
				it should behave like compound { ((p1, p2).tupled, (p3, p4).tupled).tupled -> List(p1, p2, p3, p4) }
			}
			describe("(p1, (p2, p3), p4)") {
				it should behave like compound { (p1, (p2, p3).tupled, p4).tupled -> List(p1, p2, p3, p4) }
			}

			it("should be able to combine parsers with varying (but common) input types") {
				// just making sure you can still call `tupled` when the types aren't super-explicit
				val pA: Parser[Any, Int] = Parser.pure(42)
				val pB: Parser[Int, List[Int]] = Parser.toList[Int]
				val pC = new impl.ParserDefer(() => pA)
				val parser = (pA, pB, pC).tupled
				parser.parseSeq(List(1, 2, 3)) shouldEqual ((42, List(1, 2, 3), 42))
			}
		}
	}

}
