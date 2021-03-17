package io.dylemma.spac

import cats.effect.SyncIO
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class TransformerCompanionTests extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks {

	describe("Transformer.op") {
		def opTransformer(makeTransformer: (Int => Emit[Int]) => Transformer[SyncIO, Int, Int]) = {
			val t0 = makeTransformer(_ => Emit.nil)
			val t1 = makeTransformer(i => Emit.one(i))
			val t2 = makeTransformer(i => Emit(i * 2, i * 2 + 1))

			it("should transform the input via the provided function, and be stateless") {
				forAll { (list: List[Int]) =>
					t0.stepMany(list).unsafeRunSync()._1 shouldEqual Emit.nil
					t1.stepMany(list).unsafeRunSync()._1 shouldEqual Emit.fromSeq(list)
					t2.stepMany(list).unsafeRunSync()._1 shouldEqual Emit.fromSeq(list.flatMap(i => List(i * 2, i * 2 + 1)))
				}
			}
			it("should be stateless") {
				forAll { (list: List[Int]) =>
					t0.stepMany(list).unsafeRunSync()._2 shouldEqual Right(t0)
					t1.stepMany(list).unsafeRunSync()._2 shouldEqual Right(t1)
					t2.stepMany(list).unsafeRunSync()._2 shouldEqual Right(t2)
				}
			}
			it("should emit nothing in reaction to the end of the input") {
				t0.finish.unsafeRunSync() shouldBe empty
				t1.finish.unsafeRunSync() shouldBe empty
				t2.finish.unsafeRunSync() shouldBe empty
			}
		}

		describe("Transformer.op[F, In, Out]") {
			it should behave like opTransformer(Transformer.op)
		}
		describe("Transformer[F].op[In, Out]") {
			it should behave like opTransformer(Transformer[SyncIO].op)
		}
		describe("Transformer[F, In].op[Out]") {
			it should behave like opTransformer(Transformer[SyncIO, Int].op)
		}
	}

	describe("Transformer.map") {
		def mapTransformer(makeTransformer: (Int => Int) => Transformer[SyncIO, Int, Int]) = {
			val t = makeTransformer(_ * 2)

			it("should transform the inputs via the provided function") {
				forAll { (list: List[Int]) =>
					val mappedList = Emit.fromSeq(list.map(_ * 2))
					t.stepMany(list).unsafeRunSync()._1 shouldEqual mappedList
				}
			}
			it("should be stateless") {
				forAll { (list: List[Int]) =>
					t.stepMany(list).unsafeRunSync()._2 shouldEqual Right(t)
				}
			}
			it("should emit nothing in reaction to the end of the input") {
				t.finish.unsafeRunSync() shouldBe empty
			}
		}

		describe("Transformer.map[F, In, Out]") {
			it should behave like mapTransformer(Transformer.map)
		}
		describe("Transformer[F].map[In, Out]") {
			it should behave like mapTransformer(Transformer[SyncIO].map)
		}
		describe("Transformer[F, In].map[Out]") {
			it should behave like mapTransformer(Transformer[SyncIO, Int].map)
		}
	}

	describe("Transformer.filter") {
		def filterTransformer(makeTransformer: (Int => Boolean) => Transformer[SyncIO, Int, Int]) = {
			val t = makeTransformer(_ % 2 == 0)

			it("should pass through only the inputs for which the filter function returns true") {
				forAll { (list: List[Int]) =>
					val evens = Emit.fromSeq(list.filter(_ % 2 == 0))
					t.stepMany(list).unsafeRunSync()._1 shouldEqual evens
				}
			}
			it("should be stateless") {
				forAll { (list: List[Int]) =>
					t.stepMany(list).unsafeRunSync()._2 shouldEqual Right(t)
				}
			}
			it("should emit nothing in reaction to the end of the input") {
				t.finish.unsafeRunSync() shouldBe empty
			}
		}

		describe("Transformer.filter[F, In]") {
			it should behave like filterTransformer(Transformer.filter)
		}
		describe("Transformer[F].filter[In]") {
			it should behave like filterTransformer(Transformer[SyncIO].filter)
		}
		describe("Transformer[F, In].filter") {
			it should behave like filterTransformer(Transformer[SyncIO, Int].filter)
		}
	}

	describe("Transformer.take") {
		def takeTransformer(makeTransformer: Int => Transformer[SyncIO, Int, Int]) = {
			it ("should emit outputs corresponding to List.take(n)") {
				val t0 = makeTransformer(0)
				val t1 = makeTransformer(1)
				val t5 = makeTransformer(5)

				forAll { (list: List[Int]) =>
					t0.stepMany(list).unsafeRunSync()._1 shouldEqual Emit.nil
					t1.stepMany(list).unsafeRunSync()._1 shouldEqual Emit.fromSeq(list take 1)
					t5.stepMany(list).unsafeRunSync()._1 shouldEqual Emit.fromSeq(list take 5)
				}
			}
			it ("should end upon encountering the 1st input, if N=0") {
				makeTransformer(0).step(42).unsafeRunSync() shouldEqual { Emit.nil -> None }
			}
			it ("should end after emitting Nth input, if N>0") {
				makeTransformer(1).stepMany(List(1, 2, 3)).unsafeRunSync() shouldEqual { Emit.one(1) -> Left(List(2, 3)) }
				makeTransformer(2).stepMany(List(1, 2, 3)).unsafeRunSync() shouldEqual { Emit(1, 2) -> Left(List(3)) }
				makeTransformer(3).stepMany(List(1, 2, 3)).unsafeRunSync() shouldEqual { Emit(1, 2, 3) -> Left(Nil) }
			}
			it ("should not end before encountering N inputs") {
				val t = makeTransformer(4)
				t.stepMany(List(1, 2, 3)).unsafeRunSync() should matchPattern { case (Emit(1, 2, 3), Right(_)) => }
			}
			it ("should emit nothing in reaction to the end of the input") {
				forAll { (n: Int) =>
					whenever(n >= 0) {
						makeTransformer(n).finish.unsafeRunSync() shouldEqual Emit.nil
					}
				}
			}
		}

		describe("Transformer.take[F, In]") {
			it should behave like takeTransformer(Transformer.take)
		}
		describe("Transformer[F].take[In]") {
			it should behave like takeTransformer(Transformer[SyncIO].take)
		}
		describe("Transformer[F, In].take") {
			it should behave like takeTransformer(Transformer[SyncIO, Int].take)
		}
	}

	describe("Transformer.takeWhile") {
		def takeWhileTransformer(makeTransformer: (Int => Boolean) => Transformer[SyncIO, Int, Int]) = {
			it ("should emit outputs corresponding to List.takeWhile(f)") {
				val f2 = (i: Int) => i % 2 != 0
				val f5 = (i: Int) => i % 5 != 0
				val f10 = (i: Int) => i % 10 != 0
				val t2 = makeTransformer(f2)
				val t5 = makeTransformer(f5)
				val t10 = makeTransformer(f10)

				forAll { (list: List[Int]) =>
					t2.stepMany(list).unsafeRunSync()._1 shouldEqual Emit.fromSeq(list takeWhile f2)
					t5.stepMany(list).unsafeRunSync()._1 shouldEqual Emit.fromSeq(list takeWhile f5)
					t10.stepMany(list).unsafeRunSync()._1 shouldEqual Emit.fromSeq(list takeWhile f10)
				}
			}
			it ("should discard the first predicate-failing input and terminate as a result") {
				val t = makeTransformer(_ != 0)
				t.stepMany(List(5, 4, 3, 2, 1, 0, 10)).unsafeRunSync() shouldEqual { Emit(5, 4, 3, 2, 1) -> Left(List(10)) }
				t.stepMany(List(0, 1, 2, 3)).unsafeRunSync() shouldEqual { Emit.nil -> Left(List(1, 2, 3)) }
			}
			it ("should not end if no inputs fail the predicate") {
				val t = makeTransformer(_ != 0)
				forAll { (_list: List[Int]) =>
					val chain = Emit.fromSeq(_list.filter(_ != 0))
					val (emitted, cont) = t.stepMany(chain).unsafeRunSync()
					emitted shouldEqual chain
					cont should matchPattern { case Right(_) => }
				}
			}
			it ("should be stateless") {
				val t = makeTransformer(_ != 0)
				forAll { (_list: List[Int]) =>
					val chain = Emit.fromSeq(_list.filter(_ != 0))
					t.stepMany(chain).unsafeRunSync()._2 shouldEqual Right(t)
				}
			}
			it ("should emit nothing in response to the end of the input") {
				forAll { (f: Int => Boolean) =>
					makeTransformer(f).finish.unsafeRunSync() shouldEqual Emit.nil
				}
			}
		}

		describe("Transformer.takeWhile[F, In]") {
			it should behave like takeWhileTransformer(Transformer.takeWhile)
		}
		describe("Transformer[F].takeWhile[In]") {
			it should behave like takeWhileTransformer(Transformer[SyncIO].takeWhile)
		}
		describe("Transformer[F, In].takeWhile") {
			it should behave like takeWhileTransformer(Transformer[SyncIO, Int].takeWhile)
		}
	}
}
