package io.dylemma.spac

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class TransformerCompanionTests extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks with SpacTestUtils {

	describe("Transformer.identity") {
		def identityTransformer(makeTransformer: => Transformer[Int, Int]) = {
			val t = makeTransformer

			it ("should emit each input") {
				forAll { (list: List[Int]) =>
					stepMany(t.newHandler, list).outputs shouldEqual list
				}
			}
			it ("should be stateless") {
				forAll { (list: List[Int]) =>
					forAll { (list: List[Int]) =>
						stepMany(t.newHandler, list).signal shouldEqual Signal.Continue
					}
				}
			}
			it ("should emit nothing in response to the end of the input") {
				collectFinish(t.newHandler) shouldEqual Nil
			}
		}

		describe("Transformer.identity[In]") {
			it should behave like identityTransformer(Transformer.identity[Int])
		}
		describe("Transformer[In].identity") {
			it should behave like identityTransformer(Transformer[Int].identity)
		}
	}

	describe("Transformer.mapFlatten") {
		def opTransformer(makeTransformer: (Int => Iterable[Int]) => Transformer[Int, Int]) = {
			val t0 = makeTransformer(_ => Nil)
			val t1 = makeTransformer(i => List(i))
			val t2 = makeTransformer(i => List(i * 2, i * 2 + 1))

			it("should transform the input via the provided function, and be stateless") {
				forAll { (list: List[Int]) =>
					stepMany(t0.newHandler, list).outputs shouldEqual Nil
					stepMany(t1.newHandler, list).outputs shouldEqual list
					stepMany(t2.newHandler, list).outputs shouldEqual list.flatMap(i => List(i * 2, i * 2 + 1))
				}
			}
			it("should be stateless") {
				forAll { (list: List[Int]) =>
					stepMany(t0.newHandler, list).signal shouldEqual Signal.Continue
					stepMany(t1.newHandler, list).signal shouldEqual Signal.Continue
					stepMany(t2.newHandler, list).signal shouldEqual Signal.Continue
				}
			}
			it("should emit nothing in reaction to the end of the input") {
				collectFinish(t0.newHandler) shouldBe empty
				collectFinish(t1.newHandler) shouldBe empty
				collectFinish(t2.newHandler) shouldBe empty
			}
		}

		describe("Transformer.mapFlatten[In, Out]") {
			it should behave like opTransformer(Transformer.mapFlatten)
		}
		describe("Transformer[In].mapFlatten[Out]") {
			it should behave like opTransformer(Transformer[Int].mapFlatten)
		}
	}

	describe("Transformer.map") {
		def mapTransformer(makeTransformer: (Int => Int) => Transformer[Int, Int]) = {
			val t = makeTransformer(_ * 2)

			it("should transform the inputs via the provided function") {
				forAll { (list: List[Int]) =>
					val mappedList = list.map(_ * 2)
					stepMany(t.newHandler, list).outputs shouldEqual mappedList
				}
			}
			it("should be stateless") {
				forAll { (list: List[Int]) =>
					stepMany(t.newHandler, list).signal shouldEqual Signal.Continue
				}
			}
			it("should emit nothing in reaction to the end of the input") {
				collectFinish(t.newHandler) shouldBe empty
			}
		}

		describe("Transformer.map[In, Out]") {
			it should behave like mapTransformer(Transformer.map)
		}
		describe("Transformer[In].map[Out]") {
			it should behave like mapTransformer(Transformer[Int].map)
		}
	}

	describe("Transformer.filter") {
		def filterTransformer(makeTransformer: (Int => Boolean) => Transformer[Int, Int]) = {
			val t = makeTransformer(_ % 2 == 0)

			it("should pass through only the inputs for which the filter function returns true") {
				forAll { (list: List[Int]) =>
					val evens = list.filter(_ % 2 == 0)
					stepMany(t.newHandler, list).outputs shouldEqual evens
				}
			}
			it("should be stateless") {
				forAll { (list: List[Int]) =>
					stepMany(t.newHandler, list).signal shouldEqual Signal.Continue
				}
			}
			it("should emit nothing in reaction to the end of the input") {
				collectFinish(t.newHandler) shouldBe empty
			}
		}

		describe("Transformer.filter[In]") {
			it should behave like filterTransformer(Transformer.filter)
		}
		describe("Transformer[In].filter") {
			it should behave like filterTransformer(Transformer[Int].filter)
		}
	}

	describe("Transformer.drop") {
		def dropTransformer(makeTransformer: Int => Transformer[Int, Int]) = {
			it ("should emit outputs corresponding to List.drop(n)") {
				forAll { (list: List[Int], n: Int) =>
					val t = makeTransformer(n)
					stepMany(t.newHandler, list).outputs shouldEqual list.drop(n)
				}
			}
			it ("should not end on its own") {
				var h = makeTransformer(9999).newHandler
				forAll { (i: Int) =>
					val step = stepMany(h, i :: Nil)
					step.signal shouldEqual Signal.Continue
				}
			}
			it ("should emit nothing in response to the EOF") {
				forAll { (list: List[Int], n: Int) =>
					val h = makeTransformer(n).newHandler
					h.pushMany(list.iterator, Transformer.BoundHandler.noopAndContinue) shouldEqual Signal.Continue
					collectFinish(h) shouldBe empty
				}
			}
		}

		describe("Transformer[In].drop") {
			it should behave like dropTransformer(Transformer[Int].drop)
		}
		describe("Transformer.drop[In]") {
			it should behave like dropTransformer(Transformer.drop[Int])
		}
	}

	describe("Transformer.dropWhile") {
		def dropWhileTransformer(makeTransformer: (Int => Boolean) => Transformer[Int, Int]) = {
			it ("should emit outputs corresponding to List.dropWhile(n)") {
				forAll { (list: List[Int], f: Int => Boolean) =>
					val t = makeTransformer(f)
					stepMany(t.newHandler, list).outputs shouldEqual (list dropWhile f)
				}
			}
			it ("should not end on its own") {
				var h = makeTransformer(_ => true).newHandler
				forAll { (i: Int) =>
					stepMany(h, i :: Nil).signal shouldEqual Signal.Continue
				}
			}
			it ("should emit nothing in response to the EOF") {
				forAll { (list: List[Int], f: Int => Boolean) =>
					val h = makeTransformer(f).newHandler
					stepMany(h, list).signal shouldEqual Signal.Continue
					collectFinish(h) shouldBe empty
				}
			}
		}

		describe("Transformer[In].dropWhile") {
			it should behave like dropWhileTransformer(Transformer[Int].dropWhile)
		}
		describe("Transformer.dropWhile[In]") {
			it should behave like dropWhileTransformer(Transformer.dropWhile[Int])
		}
	}

	describe("Transformer.take") {
		def takeTransformer(makeTransformer: Int => Transformer[Int, Int]) = {
			it ("should emit outputs corresponding to List.take(n)") {
				val t0 = makeTransformer(0)
				val t1 = makeTransformer(1)
				val t5 = makeTransformer(5)

				forAll { (list: List[Int]) =>
					stepMany(t0.newHandler, list).outputs shouldEqual Nil
					stepMany(t1.newHandler, list).outputs shouldEqual (list take 1)
					stepMany(t5.newHandler, list).outputs shouldEqual (list take 5)
				}
			}
			it ("should end upon encountering the 1st input, if N=0") {
				stepMany(makeTransformer(0).newHandler, 42 :: Nil) shouldEqual HandlerStep(Nil, Signal.Stop)
			}
			it ("should end after emitting Nth input, if N>0") {
				stepMany(makeTransformer(1).newHandler, List(1, 2, 3)) shouldEqual HandlerStep(List(1), Signal.Stop)
				stepMany(makeTransformer(2).newHandler, List(1, 2, 3)) shouldEqual HandlerStep(List(1, 2), Signal.Stop)
				stepMany(makeTransformer(3).newHandler, List(1, 2, 3)) shouldEqual HandlerStep(List(1, 2, 3), Signal.Stop)
			}
			it ("should not end before encountering N inputs") {
				val t = makeTransformer(4)
				stepMany(t.newHandler, List(1, 2, 3)) shouldEqual HandlerStep(List(1, 2, 3), Signal.Continue)
			}
			it ("should emit nothing in reaction to the end of the input") {
				forAll { (n: Int) =>
					whenever(n >= 0) {
						collectFinish(makeTransformer(n).newHandler) shouldEqual Nil
					}
				}
			}
		}

		describe("Transformer.take[In]") {
			it should behave like takeTransformer(Transformer.take)
		}
		describe("Transformer[In].take") {
			it should behave like takeTransformer(Transformer[Int].take)
		}
	}

	describe("Transformer.takeWhile") {
		def takeWhileTransformer(makeTransformer: (Int => Boolean) => Transformer[Int, Int]) = {
			it ("should emit outputs corresponding to List.takeWhile(f)") {
				val f2 = (i: Int) => i % 2 != 0
				val f5 = (i: Int) => i % 5 != 0
				val f10 = (i: Int) => i % 10 != 0
				val t2 = makeTransformer(f2)
				val t5 = makeTransformer(f5)
				val t10 = makeTransformer(f10)

				forAll { (list: List[Int]) =>
					stepMany(t2.newHandler, list).outputs shouldEqual (list takeWhile f2)
					stepMany(t5.newHandler, list).outputs shouldEqual (list takeWhile f5)
					stepMany(t10.newHandler, list).outputs shouldEqual (list takeWhile f10)
				}
			}
			it ("should discard the first predicate-failing input and terminate as a result") {
				val t = makeTransformer(_ != 0)
				stepMany(t.newHandler, List(5, 4, 3, 2, 1, 0, 10)) shouldEqual HandlerStep(List(5, 4, 3, 2, 1), Signal.Stop)
				stepMany(t.newHandler, List(0, 1, 2, 3)) shouldEqual HandlerStep(Nil, Signal.Stop)
			}
			it ("should not end if no inputs fail the predicate") {
				val t = makeTransformer(_ != 0)
				forAll { (_list: List[Int]) =>
					val list = _list.filter(_ != 0)
					stepMany(t.newHandler, list) shouldEqual HandlerStep(list, Signal.Continue)
				}
			}
			it ("should be stateless") {
				val t = makeTransformer(_ != 0)
				forAll { (_list: List[Int]) =>
					val list = _list.filter(_ != 0)
					stepMany(t.newHandler, list).signal shouldEqual Signal.Continue
				}
			}
			it ("should emit nothing in response to the end of the input") {
				forAll { (f: Int => Boolean) =>
					collectFinish(makeTransformer(f).newHandler) shouldEqual Nil
				}
			}
		}

		describe("Transformer.takeWhile[In]") {
			it should behave like takeWhileTransformer(Transformer.takeWhile)
		}
		describe("Transformer[In].takeWhile") {
			it should behave like takeWhileTransformer(Transformer[Int].takeWhile)
		}
	}

	describe("Transformer.tap") {
		def tapTransformer(makeTransformer: (Int => Unit) => Transformer[Int, Int]) = {
			it ("should emit every input as output") {
				val h = makeTransformer(_ => ()).newHandler
				forAll { (i: Int) =>
					stepMany(h, i :: Nil).outputs shouldEqual List(i)
				}
			}
			it ("should run the side-effect for each input") {
				forAll { (list: List[Int]) =>
					val tapped = collection.mutable.Set.empty[Int]
					val h = makeTransformer(tapped.add).newHandler
					h.pushMany(list.iterator, Transformer.BoundHandler.noopAndContinue)
					tapped.toSet shouldEqual list.toSet
				}
			}
			it ("should noop in response to the EOF") {
				forAll { (list: List[Int]) =>
					var tapped = false
					val h = makeTransformer(_ => tapped = true).newHandler
					h.pushMany(list.iterator, Transformer.BoundHandler.noopAndContinue)
					tapped shouldEqual list.nonEmpty
					tapped = false
					collectFinish(h) shouldBe empty
					tapped shouldBe false
				}
			}
		}

		describe("Transformer.tap[In]") {
			it should behave like tapTransformer(Transformer.tap[Int])
		}
		describe("Transformer[Int].tap") {
			it should behave like tapTransformer(Transformer[Int].tap)
		}
	}
}
