package io.dylemma.spac

import cats.data.Chain
import cats.effect.SyncIO
import org.scalacheck.Arbitrary
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.collection.mutable
import scala.reflect.ClassTag

class ParserTests extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks with SpacTestUtils {

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
			it should behave like firstOrErrorParser(Parser.firstOrError[SyncIO, Int, Throwable](_))
		}
		describe("Parser[F, In].firstOrError[Err]") {
			it should behave like firstOrErrorParser(Parser[SyncIO, Int].firstOrError[Throwable](_))
		}
		describe("Parser[F].firstOrError[In, Err]") {
			it should behave like firstOrErrorParser(Parser[SyncIO].firstOrError[Int, Throwable](_))
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
				assertThrows[MissingFirstException[In]] {
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
		describe("Parser.findEval[F, In]") {
			it should behave like findEvalParser(Parser.findEval[SyncIO, Int])
		}
		describe("Parser[F].findEval[In]") {
			it should behave like findEvalParser(Parser[SyncIO].findEval[Int])
		}
		describe("Parser[F, In].findEval") {
			it should behave like findEvalParser(Parser[SyncIO, Int].findEval)
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
		describe("Parser.foldEval[F, In, Out]") {
			it should behave like foldEvalParser(Parser.foldEval[SyncIO, Int, String])
		}
		describe("Parser[F].foldEval[In, Out]") {
			it should behave like foldEvalParser(Parser[SyncIO].foldEval[Int, String])
		}
		describe("Parser[F, In].foldEval[Out]") {
			it should behave like foldEvalParser(Parser[SyncIO, Int].foldEval[String])
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
		describe("Parser.toChain[F, In]") {
			it should behave like toChainParser(Parser.toChain[SyncIO, Int])
		}
		describe("Parser[F].toChain[In]") {
			it should behave like toChainParser(Parser[SyncIO].toChain[Int])
		}
		describe("Parser[F, In].toChain") {
			it should behave like toChainParser(Parser[SyncIO, Int].toChain)
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
	}

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
}
