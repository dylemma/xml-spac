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
		def firstOptParser[In: Arbitrary](parser: Parser[SyncIO, In, Option[In]]) = {
			it("should return the first element of the source stream") {
				forAll { (list: List[In]) =>
					parser.parseSeq(list).unsafeRunSync() shouldEqual list.headOption
				}
			}
			it("should return None if the source stream is empty") {
				parser.parseSeq(Nil).unsafeRunSync() shouldEqual None
			}
			it("`.parseSeq` should consume exactly one element from the source sequence") {
				countPullsIn[In] { parser.parseSeq(_).unsafeRunSync() } shouldEqual 1
			}
			it("`.parse` should consume exactly one element from the source stream") {
				countPullsIn[In] { parser.parse(_).unsafeRunSync() } shouldEqual 1
			}
		}

		describe("Parser.firstOpt[F, A]") {
			it should behave like firstOptParser(Parser.firstOpt[SyncIO, Int])
		}
		describe("Parser[F, A].firstOpt") {
			it should behave like firstOptParser(Parser[SyncIO, Int].firstOpt)
		}
		describe("Parser[F].firstOpt[A]") {
			it should behave like firstOptParser(Parser[SyncIO].firstOpt[Int])
		}
		describe("Parser.over[A].firstOpt[F]") {
			it should behave like firstOptParser(Parser.over[Int].firstOpt[SyncIO])
		}
	}

	describe("Parser.firstOrError") {
		val dummyException = new Exception("oh no")

		def firstOrErrorParser[In: Arbitrary](makeParser: Throwable => Parser[SyncIO, In, In]) = {
			val parser = makeParser(dummyException)
			it("should return the first element of a non-empty source") {
				forAll { (list: List[In]) =>
					whenever(list.nonEmpty) {
						parser.parseSeq(list).unsafeRunSync() shouldEqual list.head
					}
				}
			}
			it("should return the error value when parsing an empty sequence") {
				intercept[Exception] { parser.parseSeq(Nil).unsafeRunSync() } shouldEqual dummyException
			}
			it("`.parseSeq` should consume exactly one element from the source sequence") {
				countPullsIn[In] { parser.parseSeq(_).unsafeRunSync() } shouldEqual 1
			}
			it("`.parse` should consume exactly one element from the source stream") {
				countPullsIn[In] { parser.parse(_).unsafeRunSync() } shouldEqual 1
			}
		}

		describe("Parser.firstOrError[F, In, Err]") {
			it should behave like firstOrErrorParser(Parser.firstOrError[SyncIO, Int, Throwable])
		}
		describe("Parser[F, In].firstOrError[Err]") {
			it should behave like firstOrErrorParser(Parser[SyncIO, Int].firstOrError[Throwable])
		}
		describe("Parser[F].firstOrError[In, Err]") {
			it should behave like firstOrErrorParser(Parser[SyncIO].firstOrError[Int, Throwable])
		}
		describe("Parser.over[In].firstOrError[F, Err]") {
			it should behave like firstOrErrorParser(Parser.over[Int].firstOrError[SyncIO, Throwable])
		}
	}

	describe("Parser.first") {
		def firstParser[In: Arbitrary : ClassTag](parser: Parser[SyncIO, In, In]) = {
			it("should return the first element of a non-empty source") {
				forAll { (list: List[In]) =>
					whenever(list.nonEmpty) {
						parser.parseSeq(list).unsafeRunSync() shouldEqual list.head
					}
				}
			}
			it("should throw a MissingFirstException when parsing an empty sequence") {
				assertThrows[SpacException.MissingFirstException[In]] {
					parser.parseSeq(Nil).unsafeRunSync()
				}
			}
			it("`.parseSeq` should consume exactly one element from the source sequence") {
				countPullsIn[In] { parser.parseSeq(_).unsafeRunSync() } shouldEqual 1
			}
			it("`.parse` should consume exactly one element from the source stream") {
				countPullsIn[In] { parser.parse(_).unsafeRunSync() } shouldEqual 1
			}
		}

		describe("Parser.first[F, In]") {
			it should behave like firstParser(Parser.first[SyncIO, Int])
		}
		describe("Parser[F].first[In]") {
			it should behave like firstParser(Parser[SyncIO].first[Int])
		}
		describe("Parser[F, In].first") {
			it should behave like firstParser(Parser[SyncIO, Int].first)
		}
		describe("Parser.over[In].first[F]") {
			it should behave like firstParser(Parser.over[Int].first[SyncIO])
		}
	}

	describe("Parser.find / Parser.findEval") {
		val dummyException = new Exception("oh no")

		def findEvalParser(makeParser: (Int => SyncIO[Boolean]) => Parser[SyncIO, Int, Option[Int]]) = {
			val parser = makeParser(i => SyncIO { i % 2 == 0 })
			val errorProneParser = makeParser {
				case 0 => SyncIO { throw dummyException }
				case 1 => SyncIO { true }
				case _ => SyncIO { false }
			}
			_findParserBehavior(parser, errorProneParser)
		}

		def findParser(makeParser: (Int => Boolean) => Parser[SyncIO, Int, Option[Int]]) = {
			val parser = makeParser(_ % 2 == 0)
			val errorProneParser = makeParser {
				case 0 => throw dummyException
				case 1 => true
				case _ => false
			}
			_findParserBehavior(parser, errorProneParser)
		}

		def _findParserBehavior(
			parser: Parser[SyncIO, Int, Option[Int]],
			errorProneParser: Parser[SyncIO, Int, Option[Int]],
		) = {
			it("should return None when parsing an empty list") {
				parser.parseSeq(Nil).unsafeRunSync() shouldEqual None
			}
			it("should return Some when parsing a list which contains a matching value") {
				parser.parseSeq(List(2)).unsafeRunSync() shouldEqual Some(2)
			}
			it("should return None when parsing a list which does not contain any matching value") {
				parser.parseSeq(List(1, 3, 5, 7, 9)).unsafeRunSync() shouldEqual None
			}
			describe(".parseSeq") {
				it("should not pull values past the point where a matching value is found") {
					countPullsIn(List(1, 2, 3, 4)) { parser.parseSeq(_).unsafeRunSync() } shouldEqual 2
				}
				it("should pull the entire input if no matching value is found") {
					countPullsIn(List(1, 3, 5, 7)) { parser.parseSeq(_).unsafeRunSync() } shouldEqual 5
				}
			}
			describe(".parse") {
				it("should not pull values past the point where a matching value is found") {
					countPullsIn(List(1, 2, 3, 4)) { parser.parse(_).unsafeRunSync() } shouldEqual 2
				}
				it("should pull the entire input if no matching value is found") {
					countPullsIn(List(1, 3, 5, 7)) { parser.parse(_).unsafeRunSync() } shouldEqual 5
				}
			}
			describe("with an exception-throwing predicate") {
				it("should capture thrown exceptions in the effect context") {
					val result = errorProneParser.parse(List(0)).attempt.unsafeRunSync()
					result shouldEqual Left(dummyException)
				}
				it("should stop pulling values from the source when an exception is thrown") {
					countPullsIn(List(3, 2, 0, 1)) { errorProneParser.parse(_).attempt.unsafeRunSync() } shouldEqual 3
				}
			}
		}

		describe("Parser.find[F, In]") {
			it should behave like findParser(Parser.find[SyncIO, Int])
		}
		describe("Parser[F].find[In]") {
			it should behave like findParser(Parser[SyncIO].find[Int])
		}
		describe("Parser[F, In].find") {
			it should behave like findParser(Parser[SyncIO, Int].find)
		}
		describe("Parser.over[In].find[F]") {
			it should behave like findParser(Parser.over[Int].find[SyncIO])
		}
		describe("Parser.findEval[F, In]") {
			it should behave like findEvalParser(Parser.findEval[SyncIO, Int])
		}
		describe("Parser[F].findEval[In]") {
			it should behave like findEvalParser(Parser[SyncIO].findEval[Int])
		}
		describe("Parser[F, In].findEval") {
			it should behave like findEvalParser(Parser[SyncIO, Int].findEval)
		}
		describe("Parser.over[In].findEval[F]") {
			it should behave like findEvalParser(Parser.over[Int].findEval[SyncIO])
		}
	}

	describe("Parser.fold / Parser.foldEval") {
		val dummyException = new Exception("oh no, fold failure")

		def foldParser(makeParser: String => ((String, Int) => String) => Parser[SyncIO, Int, String]) = {
			val parser = makeParser("") { (accum, next) => accum + next }
			val errorProneParser = makeParser("") {
				case (accum, 0) => throw dummyException
				case (accum, i) => accum + i
			}
			_foldBehavior(parser, errorProneParser)
		}

		def foldEvalParser(makeParser: String => ((String, Int) => SyncIO[String]) => Parser[SyncIO, Int, String]) = {
			val parser = makeParser("") { (accum, next) => SyncIO { accum + next } }
			val errorProneParser = makeParser("") {
				case (accum, 0) => SyncIO { throw dummyException }
				case (accum, i) => SyncIO { accum + i }
			}
			_foldBehavior(parser, errorProneParser)
		}

		def _foldBehavior(
			parser: Parser[SyncIO, Int, String],
			errorProneParser: Parser[SyncIO, Int, String],
		) = {
			it("should return the initial value when parsing an empty list") {
				parser.parseSeq(Nil).unsafeRunSync() shouldEqual ""
			}
			it("should aggregate the inputs in the order they are encountered") {
				parser.parseSeq(List(4, 2, 0, 6, 9)).unsafeRunSync() shouldEqual "42069"
			}
			describe("with an exception-throwing aggregator function") {
				it("should capture thrown exceptions in the effect context") {
					errorProneParser.parseSeq(List(3, 2, 1, 0)).attempt.unsafeRunSync() shouldEqual Left(dummyException)
				}
				it("should stop consuming inputs after an exception is thrown") {
					countPullsIn(List(3, 2, 1, 0, 1, 2, 3)) {
						errorProneParser.parseSeq(_).attempt.unsafeRunSync()
					} shouldEqual 4
				}
			}
			it("`.parse` should pull all inputs") {
				countPullsIn(List(1, 2, 3, 4)) { parser.parse(_).unsafeRunSync() } shouldEqual 5
				countPullsIn[List, Int](Nil) { parser.parse(_).unsafeRunSync() } shouldEqual 1
			}
			it("`.parseSeq` should pull all inputs") {
				countPullsIn(List(1, 2, 3, 4)) { parser.parseSeq(_).unsafeRunSync() } shouldEqual 5
				countPullsIn[List, Int](Nil) { parser.parseSeq(_).unsafeRunSync() } shouldEqual 1
			}
		}

		describe("Parser.fold[F, In, Out]") {
			it should behave like foldParser(Parser.fold[SyncIO, Int, String])
		}
		describe("Parser[F].fold[In, Out]") {
			it should behave like foldParser(Parser[SyncIO].fold[Int, String])
		}
		describe("Parser[F, In].fold[Out]") {
			it should behave like foldParser(Parser[SyncIO, Int].fold[String])
		}
		describe("Parser.over[In].fold[F, Out]") {
			it should behave like foldParser(Parser.over[Int].fold[SyncIO, String])
		}
		describe("Parser.foldEval[F, In, Out]") {
			it should behave like foldEvalParser(Parser.foldEval[SyncIO, Int, String])
		}
		describe("Parser[F].foldEval[In, Out]") {
			it should behave like foldEvalParser(Parser[SyncIO].foldEval[Int, String])
		}
		describe("Parser[F, In].foldEval[Out]") {
			it should behave like foldEvalParser(Parser[SyncIO, Int].foldEval[String])
		}
		describe("Parser.over[In].foldEval[F, Out]") {
			it should behave like foldEvalParser(Parser.over[Int].foldEval[SyncIO, String])
		}
	}

	describe("Parser.pure") {
		def pureParser(makeParser: Int => Parser[SyncIO, Any, Int]) = {
			val parser = makeParser(42)
			it("should return the value and ignore the inputs from the source") {
				forAll { (list: List[Int]) =>
					parser.parseSeq(list).unsafeRunSync() shouldEqual 42
				}
			}
			it("should return the wrapped value even if the source is empty") {
				parser.parseSeq(Nil).unsafeRunSync() shouldEqual 42
			}
			it("`.parseSeq` should pull only input") {
				countPullsIn[Int] { parser.parseSeq(_).unsafeRunSync() } shouldEqual 1
			}
			it("`.parse` should pull only 1 input") {
				countPullsIn[Int] { parser.parse(_).unsafeRunSync() } shouldEqual 1
			}
		}

		describe("Parser.pure[F, Out]") {
			it should behave like pureParser(Parser.pure[SyncIO, Int])
		}
		describe("Parser[F].pure[Out]") {
			it should behave like pureParser(Parser[SyncIO].pure[Int])
		}
		describe("Parser[F, In].pure[Out]") {
			it should behave like pureParser(Parser[SyncIO, Any].pure[Int])
		}
		describe("Parser.over[In].pure[F, Out]") {
			it should behave like pureParser(Parser.over[Any].pure[SyncIO, Int])
		}
	}

	describe("Parser.eval") {
		def evalParser(makeParser: SyncIO[Parser[SyncIO, Int, String]] => Parser[SyncIO, Int, String]) = {
			def innerParser1 = Parser[SyncIO].pure("yay")

			val parser1 = makeParser(SyncIO { innerParser1 })

			def innerParser2 = Parser[SyncIO, Int].fold("")(_ + _)

			val parser2 = makeParser(SyncIO { innerParser2 })

			it("should return the same value that the eval-returned parser would") {
				forAll { (list: List[Int]) =>
					parser1.parseSeq(list).unsafeRunSync() shouldEqual innerParser1.parseSeq(list).unsafeRunSync()
					parser2.parseSeq(list).unsafeRunSync() shouldEqual innerParser2.parseSeq(list).unsafeRunSync()
				}
			}

			it("should only pull as many values from the source as the eval-returned parser would") {
				countPullsIn(List(1, 2, 3)) { parser1.parseSeq(_).unsafeRunSync() } shouldEqual 1
				countPullsIn(List(1, 2, 3)) { parser2.parseSeq(_).unsafeRunSync() } shouldEqual 4
			}
		}

		describe("Parser.eval[F, In, Out]") {
			it should behave like evalParser(Parser.eval[SyncIO, Int, String])
		}
		describe("Parser[F].eval[In, Out]") {
			it should behave like evalParser(Parser[SyncIO].eval[Int, String])
		}
		describe("Parser[F, In].eval[Out]") {
			it should behave like evalParser(Parser[SyncIO, Int].eval[String])
		}
		describe("Parser.over[In].eval[F, Out]") {
			it should behave like evalParser(Parser.over[Int].eval[SyncIO, String])
		}
	}

	describe("Parser.toChain / Parser.toList") {
		def _collectorBehavior[C[_], A: Arbitrary](
			collectionName: String,
			parser: Parser[SyncIO, A, C[A]],
			materializeCollection: List[A] => C[A],
		) = {
			it(s"should collect all inputs from the source into a $collectionName") {
				forAll { (list: List[A]) =>
					parser.parseSeq(list).unsafeRunSync() shouldEqual materializeCollection(list)
				}
			}
			it("should always pull all inputs from the source") {
				forAll { (list: List[A]) =>
					countPullsIn(list) { parser.parseSeq(_).unsafeRunSync() } shouldEqual (list.size + 1)
				}
			}
		}

		def toListParser(parser: Parser[SyncIO, Int, List[Int]]) = {
			_collectorBehavior[List, Int]("List", parser, identity)
		}

		def toChainParser(parser: Parser[SyncIO, Int, Chain[Int]]) = {
			_collectorBehavior[Chain, Int]("Chain", parser, Chain.fromSeq)
		}

		describe("Parser.toList[F, In]") {
			it should behave like toListParser(Parser.toList[SyncIO, Int])
		}
		describe("Parser[F].toList[In]") {
			it should behave like toListParser(Parser[SyncIO].toList[Int])
		}
		describe("Parser[F, In].toList") {
			it should behave like toListParser(Parser[SyncIO, Int].toList)
		}
		describe("Parser.over[In].toList[F]") {
			it should behave like toListParser(Parser.over[Int].toList[SyncIO])
		}
		describe("Parser.toChain[F, In]") {
			it should behave like toChainParser(Parser.toChain[SyncIO, Int])
		}
		describe("Parser[F].toChain[In]") {
			it should behave like toChainParser(Parser[SyncIO].toChain[Int])
		}
		describe("Parser[F, In].toChain") {
			it should behave like toChainParser(Parser[SyncIO, Int].toChain)
		}
		describe("Parser.over[In].toChain[F]") {
			it should behave like toChainParser(Parser.over[Int].toChain[SyncIO])
		}
	}

	describe("Parser.impureBuild") {
		def impureBuildParser(makeParser: (=> mutable.Builder[Int, List[Int]]) => Parser[SyncIO, Int, List[Int]]) = {
			it("should add each input from the source to the builder, returning the result at the end of the stream") {
				val parser = makeParser { List.newBuilder }
				forAll { (list: List[Int]) =>
					parser.parseSeq(list).unsafeRunSync() shouldEqual list
				}
			}
			it("should construct a new builder when it is asked to parse a source, to avoid sharing builders") {
				// effectively the same test as above, but with a different motivation
				val parser = makeParser { List.newBuilder }
				val result1 = parser.parseSeq(List(1, 2, 3)).unsafeRunSync()
				val result2 = parser.parseSeq(List(1, 2, 3)).unsafeRunSync()
				result1 shouldEqual result2
				result1 shouldEqual List(1, 2, 3)
			}
			describe("when constructed with a shared builder reference") {
				it("re-running the parser will continue to accumulate values in the builder") {
					// this is what you SHOULD NOT do when using Parser.impureBuild
					val sharedBuilder = List.newBuilder[Int]
					val sharingParser = makeParser(sharedBuilder)
					val result1 = sharingParser.parseSeq(List(1, 2, 3)).unsafeRunSync()
					val result2 = sharingParser.parseSeq(List(4, 5, 6)).unsafeRunSync()
					result1 shouldEqual List(1, 2, 3)
					result2 shouldEqual List(1, 2, 3, 4, 5, 6) // i.e. be careful not to share builders!
				}
			}
		}

		describe("Parser.impureBuild[F, In, Out]") {
			it should behave like impureBuildParser(Parser.impureBuild[SyncIO, Int, List[Int]])
		}
		describe("Parser[F].impureBuild[In, Out]") {
			it should behave like impureBuildParser(Parser[SyncIO].impureBuild[Int, List[Int]])
		}
		describe("Parser[F, In].impureBuild[Out]") {
			it should behave like impureBuildParser(Parser[SyncIO, Int].impureBuild[List[Int]])
		}
		describe("Parser.over[In].impureBuild[F, Out]") {
			it should behave like impureBuildParser(Parser.over[Int].impureBuild[SyncIO, List[Int]])
		}
	}



	describe("Applicative[Parser]") {
		val F = Applicative[Parser[SyncIO, Int, *]]

		val p1 = Parser[SyncIO, Int].toList.withName("P1")
		val p2 = Parser[SyncIO, Int].firstOpt.withName("P2")
		val p3 = Parser[SyncIO, Int].fold("")(_ + _).withName("P3")
		val p4 = Parser[SyncIO, Int].pure("hello").withName("P4")

		val dummyException = new Exception("oh no")
		val dummyException2 = new Exception("oh yeah!")
		val pErr = Parser[SyncIO, Int].eval { SyncIO.raiseError(dummyException) }
		val pConditionalError = Parser[SyncIO, Int].fold("") {
			case (_, 0) => throw dummyException2
			case (accum, next) => accum + next
		}

		def inspectCompound[F[+_], In, Out](parser: Parser[F, In, Out]) = parser match {
			case compound: ParserCompoundN[F, In, Out] => Some(compound.inspect)
			case _ => None
		}

		describe(".product") {
			it("should result in a ParserCompoundN with two underlying parsers") {
				val pN = F.product(p1, p2)
				inspectCompound(pN) shouldEqual Some {
					Vector(Right(p1), Right(p2))
				}
			}
			it("should combine the output from the underlying parsers") {
				val pN = F.product(p1, p2)
				pN.parseSeq(List(1, 2, 3)).unsafeRunSync() shouldEqual {
					List(1, 2, 3) -> Some(1)
				}
			}
			it("should only pull each event from the source once") {
				val pN = F.product(p1, p3)
				forAll { (list: List[Int]) =>
					countPullsIn(list) { pN.parseSeq(_).unsafeRunSync() } shouldEqual (list.size + 1)
				}
			}
			it("should catch and raise errors from the underlying parsers") {
				val pN = F.product(p2, pErr)
				forAll { (list: List[Int]) =>
					pN.parse(list).attempt.unsafeRunSync() shouldEqual Left(dummyException)
				}
			}
			it("should abort execution upon encountering an error from an underlying parser") {
				countPullsIn[Int] { F.product(p2, pErr).parseSeq(_).attempt.unsafeRunSync() } shouldEqual 1
				forAll { (list: List[Int]) =>
					val indexOf0 = list.indexOf(0)
					val numPullsExpected = if (indexOf0 < 0) list.size + 1 else indexOf0 + 1
					val pN = F.product(p1, pConditionalError)
					countPullsIn(list) { pN.parseSeq(_).attempt.unsafeRunSync() } shouldEqual numPullsExpected
				}
			}
		}

		describe("SemigroupalOps.tupled") {
			def compound(arg: (Parser[SyncIO, Int, Any], List[Parser[SyncIO, Int, Any]])) = {
				val (p, parsers) = arg
				it("should create a ParserCompositeN with underlying parsers in a similar order") {
					val expected = parsers.view.map(Right(_)).toVector
					inspectCompound(p) shouldEqual Some(expected)
				}

				it("should yield the same product of results as if the underlying parsers were run separately and then combined") {
					forAll { (list: List[Int]) =>
						val compoundResults = p
							.parseSeq(list)
							.attempt
							.unsafeRunSync()
							.asInstanceOf[Either[Throwable, Product]]
							.map(tryFlattenTuple)
						val expectedResults = Traverse[List]
							.sequence(parsers.map(_.parseSeq(list)))
							.attempt
							.unsafeRunSync()
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
				val pA: Parser[SyncIO, Any, Int] = Parser[SyncIO].pure(42)
				val pB: Parser[SyncIO, Int, List[Int]] = Parser[SyncIO].toList[Int]
				val pC = new impl.ParserNamed("test", pA)
				val parser = (pA, pB, pC).tupled
				parser.parseSeq(List(1, 2, 3)).unsafeRunSync() shouldEqual ((42, List(1, 2, 3), 42))
			}
		}
	}


}
