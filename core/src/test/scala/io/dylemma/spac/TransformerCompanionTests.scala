package io.dylemma.spac

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class TransformerCompanionTests extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks {

	describe("Transformer.identity") {
		def identityTransformer(makeTransformer: => Transformer[Int, Int]) = {
			val t = makeTransformer

			it ("should emit each input") {
				forAll { (list: List[Int]) =>
					t.newHandler.stepMany(list)._1 shouldEqual Emit.fromSeq(list)
				}
			}
			it ("should be stateless") {
				forAll { (list: List[Int]) =>
					forAll { (list: List[Int]) =>
						t.newHandler.stepMany(list)._2 shouldEqual Right(t)
					}
				}
			}
			it ("should emit nothing in response to the end of the input") {
				t.newHandler.finish() shouldEqual Emit.empty
			}
		}

		describe("Transformer.identity[In]") {
			it should behave like identityTransformer(Transformer.identity[Int])
		}
		describe("Transformer[In].identity") {
			it should behave like identityTransformer(Transformer[Int].identity)
		}
	}

	describe("Transformer.op") {
		def opTransformer(makeTransformer: (Int => Emit[Int]) => Transformer[Int, Int]) = {
			val t0 = makeTransformer(_ => Emit.empty)
			val t1 = makeTransformer(i => Emit.one(i))
			val t2 = makeTransformer(i => Emit(i * 2, i * 2 + 1))

			it("should transform the input via the provided function, and be stateless") {
				forAll { (list: List[Int]) =>
					t0.newHandler.stepMany(list)._1 shouldEqual Emit.empty
					t1.newHandler.stepMany(list)._1 shouldEqual Emit.fromSeq(list)
					t2.newHandler.stepMany(list)._1 shouldEqual Emit.fromSeq(list.flatMap(i => List(i * 2, i * 2 + 1)))
				}
			}
			it("should be stateless") {
				forAll { (list: List[Int]) =>
					t0.newHandler.stepMany(list)._2 shouldEqual Right(t0)
					t1.newHandler.stepMany(list)._2 shouldEqual Right(t1)
					t2.newHandler.stepMany(list)._2 shouldEqual Right(t2)
				}
			}
			it("should emit nothing in reaction to the end of the input") {
				t0.newHandler.finish() shouldBe empty
				t1.newHandler.finish() shouldBe empty
				t2.newHandler.finish() shouldBe empty
			}
		}

		describe("Transformer.op[In, Out]") {
			it should behave like opTransformer(Transformer.mapFlatten)
		}
		describe("Transformer[In].op[Out]") {
			it should behave like opTransformer(Transformer[Int].mapFlatten)
		}
	}

	describe("Transformer.map") {
		def mapTransformer(makeTransformer: (Int => Int) => Transformer[Int, Int]) = {
			val t = makeTransformer(_ * 2)

			it("should transform the inputs via the provided function") {
				forAll { (list: List[Int]) =>
					val mappedList = Emit.fromSeq(list.map(_ * 2))
					t.newHandler.stepMany(list)._1 shouldEqual mappedList
				}
			}
			it("should be stateless") {
				forAll { (list: List[Int]) =>
					t.newHandler.stepMany(list)._2 shouldEqual Right(t)
				}
			}
			it("should emit nothing in reaction to the end of the input") {
				t.newHandler.finish() shouldBe empty
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
					val evens = Emit.fromSeq(list.filter(_ % 2 == 0))
					t.newHandler.stepMany(list)._1 shouldEqual evens
				}
			}
			it("should be stateless") {
				forAll { (list: List[Int]) =>
					t.newHandler.stepMany(list)._2 shouldEqual Right(t)
				}
			}
			it("should emit nothing in reaction to the end of the input") {
				t.newHandler.finish() shouldBe empty
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
					t.newHandler.stepMany(list)._1 shouldEqual Emit.fromSeq(list drop n)
				}
			}
			it ("should not end on its own") {
				var h = makeTransformer(9999).newHandler
				forAll { (i: Int) =>
					val (_, cont) = h.step(i)
					cont.isDefined shouldEqual true
					h = cont.get
				}
			}
			it ("should emit nothing in response to the EOF") {
				forAll { (list: List[Int], n: Int) =>
					val h = makeTransformer(n).newHandler
					val (_, Right(cont)) = h.stepMany(list)
					cont.finish() shouldBe empty
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
					t.newHandler.stepMany(list)._1 shouldEqual Emit.fromSeq(list dropWhile f)
				}
			}
			it ("should not end on its own") {
				var h = makeTransformer(_ => true).newHandler
				forAll { (i: Int) =>
					val (_, cont) = h.step(i)
					cont.isDefined shouldEqual true
					h = cont.get
				}
			}
			it ("should emit nothing in response to the EOF") {
				forAll { (list: List[Int], f: Int => Boolean) =>
					val h = makeTransformer(f).newHandler
					val (_, Right(cont)) = h.stepMany(list)
					cont.finish() shouldBe empty
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
					t0.newHandler.stepMany(list)._1 shouldEqual Emit.empty
					t1.newHandler.stepMany(list)._1 shouldEqual Emit.fromSeq(list take 1)
					t5.newHandler.stepMany(list)._1 shouldEqual Emit.fromSeq(list take 5)
				}
			}
			it ("should end upon encountering the 1st input, if N=0") {
				makeTransformer(0).newHandler.step(42) shouldEqual { Emit.empty -> None }
			}
			it ("should end after emitting Nth input, if N>0") {
				makeTransformer(1).newHandler.stepMany(List(1, 2, 3)) shouldEqual { Emit.one(1) -> Left(List(2, 3)) }
				makeTransformer(2).newHandler.stepMany(List(1, 2, 3)) shouldEqual { Emit(1, 2) -> Left(List(3)) }
				makeTransformer(3).newHandler.stepMany(List(1, 2, 3)) shouldEqual { Emit(1, 2, 3) -> Left(Nil) }
			}
			it ("should not end before encountering N inputs") {
				val t = makeTransformer(4)
				t.newHandler.stepMany(List(1, 2, 3)) should matchPattern { case (Emit(1, 2, 3), Right(_)) => }
			}
			it ("should emit nothing in reaction to the end of the input") {
				forAll { (n: Int) =>
					whenever(n >= 0) {
						makeTransformer(n).newHandler.finish() shouldEqual Emit.empty
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
					t2.newHandler.stepMany(list)._1 shouldEqual Emit.fromSeq(list takeWhile f2)
					t5.newHandler.stepMany(list)._1 shouldEqual Emit.fromSeq(list takeWhile f5)
					t10.newHandler.stepMany(list)._1 shouldEqual Emit.fromSeq(list takeWhile f10)
				}
			}
			it ("should discard the first predicate-failing input and terminate as a result") {
				val t = makeTransformer(_ != 0)
				t.newHandler.stepMany(List(5, 4, 3, 2, 1, 0, 10)) shouldEqual { Emit(5, 4, 3, 2, 1) -> Left(List(10)) }
				t.newHandler.stepMany(List(0, 1, 2, 3)) shouldEqual { Emit.empty -> Left(List(1, 2, 3)) }
			}
			it ("should not end if no inputs fail the predicate") {
				val t = makeTransformer(_ != 0)
				forAll { (_list: List[Int]) =>
					val chain = Emit.fromSeq(_list.filter(_ != 0))
					val (emitted, cont) = t.newHandler.stepMany(chain)
					emitted shouldEqual chain
					cont should matchPattern { case Right(_) => }
				}
			}
			it ("should be stateless") {
				val t = makeTransformer(_ != 0)
				forAll { (_list: List[Int]) =>
					val chain = Emit.fromSeq(_list.filter(_ != 0))
					t.newHandler.stepMany(chain)._2 shouldEqual Right(t)
				}
			}
			it ("should emit nothing in response to the end of the input") {
				forAll { (f: Int => Boolean) =>
					makeTransformer(f).newHandler.finish() shouldEqual Emit.empty
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
					h.step(i)._1 shouldEqual Emit.one(i)
				}
			}
			it ("should run the side-effect for each input") {
				forAll { (list: List[Int]) =>
					val tapped = collection.mutable.Set.empty[Int]
					val h = makeTransformer(tapped.add).newHandler
					h.stepMany(list)
					tapped.toSet shouldEqual list.toSet
				}
			}
			it ("should noop in response to the EOF") {
				forAll { (list: List[Int]) =>
					var tapped = false
					val h = makeTransformer(_ => tapped = true).newHandler
					h.stepMany(list)
					tapped shouldEqual list.nonEmpty
					tapped = false
					h.finish() shouldBe empty
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
