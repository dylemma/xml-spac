package io.dylemma.spac

import io.dylemma.spac.Result.Success
import io.dylemma.spac.handlers.ManualFinish
import org.scalatest.{FunSpec, Matchers}

import scala.util.control.NonFatal

class TransformerTests extends FunSpec with Matchers {

	protected def runTransformer[T, U](inputs: List[T])(transformer: Transformer[T, U]): List[U] = {
		transformer >> Consumer.ToList[U] consume inputs
	}

	protected def enforceIsFinishedContract[A](transformers: Transformer[Int, A]*) = {
		it("should not call handle* on the downstream handler if the downstream handler is finished"){
			for(transformer <- transformers) {
				val consumer = new Consumer[A, Unit] {
					def makeHandler(): Handler[A, Unit] = new Handler[A, Unit] with ManualFinish {
						def handleEnd(): Unit = {
							if (isFinished) fail("downstream handler's handleEnd method called after it had already finished")
							else finishWith(())
						}
						def handleInput(input: A): Option[Unit] = {
							if (isFinished) fail("downstream handler's handleInput method called after it had already finished")
							else maybeFinishWith(Some(()))
						}
						def handleError(error: Throwable): Option[Unit] = {
							if (isFinished) fail("downstream handler's handleInput method called after it had already finished")
							else maybeFinishWith(Some(()))
						}
					}
				}
				try {
					transformer >> consumer consume List(1, 2, 3, 4, 5, 6, 7)
				} catch {
					case NonFatal(err) => fail(err)
				}
			}
		}
	}

	describe("Transformer.take"){
		it("should stop passing through inputs after the take limit is reached"){
			runTransformer(List(1,2,3,4,5))(Transformer.Take(2)) should be(List(1,2))
		}
		it("should end gracefully if the input ends before the take limit is reached"){
			runTransformer(Nil)(Transformer.Take(2)) should be(Nil)
			runTransformer(List(1))(Transformer.Take(2)) should be(List(1))
		}
		enforceIsFinishedContract(Transformer.Take(3))
	}

	describe("Transformer.takeWhile"){
		it("should stop passing through inputs after the condition is broken"){
			runTransformer(List(1,2,3,4,5))(Transformer.TakeWhile(_ < 10)) should be(List(1,2,3,4,5))
			runTransformer(List(1,2,3,4,5))(Transformer.TakeWhile(_ < 4)) should be(List(1,2,3))
			runTransformer(List(1,2,3,4,5))(Transformer.TakeWhile(_ < 0)) should be(Nil)
			runTransformer(List(1,2,3,4,5))(Transformer.TakeWhile(_ => false)) should be(Nil)
			runTransformer(List(1,2,3,4,5))(Transformer.TakeWhile(_ => true)) should be(List(1,2,3,4,5))
			runTransformer(List(1,2,1))(Transformer.TakeWhile(_ == 1)) should be(List(1))
		}
		it("should behave properly if the downstream handler is finished"){
			runTransformer(List(1,2,3,4,5))(Transformer.TakeWhile[Int](_ > 0) >> Transformer.Take[Int](1)) should be(List(1))
			runTransformer(List(1,2,3,4,5))(Transformer.TakeWhile[Int](_ > 0) >> Transformer.Take[Int](0)) should be(Nil)
		}
		enforceIsFinishedContract(
			Transformer.TakeWhile(_ => false),
			Transformer.TakeWhile(_ => true),
			Transformer.TakeWhile(_ < 3)
		)
	}

	describe("Transformer.Drop"){
		it("should ignore the appropriate number of inputs before passing any along"){
			runTransformer(List(1,2,3,4,5))(Transformer.Drop(0)) should be(List(1,2,3,4,5))
			runTransformer(List(1,2,3,4,5))(Transformer.Drop(3)) should be(List(4,5))
			runTransformer(List(1,2,3,4,5))(Transformer.Drop(10)) should be(Nil)
		}
		enforceIsFinishedContract(Transformer.Drop(0), Transformer.Drop(10))
	}

	describe("Transformer.DropWhile"){
		it("should ignore inputs until the condition is broken"){
			runTransformer(List(1,2,3,4,5))(Transformer.DropWhile(_ < 6)) should be(Nil)
			runTransformer(List(1,2,3,4,5))(Transformer.DropWhile(_ < 3)) should be(List(3,4,5))
			runTransformer(List(1,2,3,4,5))(Transformer.DropWhile(_ < 0)) should be(List(1,2,3,4,5))
			runTransformer(List(1,2,1))(Transformer.DropWhile(_ == 1)) should be(List(2,1))
		}
		enforceIsFinishedContract(
			Transformer.DropWhile(_ => false),
			Transformer.DropWhile(_ => true),
			Transformer.DropWhile(_ < 3)
		)
	}

	describe("Transformer.Filter"){
		it("should skip inputs that don't pass the predicate"){
			runTransformer(List(1,2,3,4,5))(Transformer.Filter(_ % 2 == 0)) should be(List(2,4))
			runTransformer(List(1,2,3,4,5))(Transformer.Filter(_ % 2 == 1)) should be(List(1,3,5))
			runTransformer(List(1,2,3,4,5))(Transformer.Filter(_ => true)) should be(List(1,2,3,4,5))
			runTransformer(List(1,2,3,4,5))(Transformer.Filter(_ => false)) should be(Nil)
		}
		enforceIsFinishedContract(
			Transformer.Filter(_ => true),
			Transformer.Filter(_ => false),
			Transformer.Filter(_ % 2 == 0)
		)
	}

	describe("Transformer.Map"){
		it("should apply a function to inputs before sending them downstream"){
			runTransformer(List(1,2,3))(Transformer.Map(_ * 2)) should be(List(2,4,6))
			runTransformer(List("1", "2"))(Transformer.Map(_.toInt)) should be(List(1,2))
		}
		it("should catch errors thrown by the function and call handleError on the downstream handler"){
			val result = Transformer.Map{s: String => s.toInt} >> Consumer.ToList[Int].safe consume List("not a number")
			result should matchPattern {
				case Result.Error(err: NumberFormatException) =>
			}
		}
		enforceIsFinishedContract(
			Transformer.Map[Int, Int](_ * 2),
			Transformer.Map[Int, Int](identity)
		)
	}

	describe("Transformer.Collect"){
		it("should transform inputs according to the collector function"){
			runTransformer(List(1,2,3,4,5))(Transformer.Collect{ case i if i % 2 == 0 => i / 2 }) should be(List(1,2))
			runTransformer(List[Int]())(Transformer.Collect { case x => x }) should be(Nil)
		}
		it("should catch errors thrown by the collector function and pass them downstream via handleError"){
			val result = Transformer.Collect[String, Int]{ case s => s.toInt } >> Consumer.ToList[Int].safe consume List("1", "2", "hi")
			result should matchPattern { case Result.Error(e: NumberFormatException) => }
		}
		enforceIsFinishedContract(
			Transformer.Collect[Int, String]{ case 1 => "hi" }
		)
	}

	describe("Transformer.Scan"){
		it("should pass its current state to the downstream, and advance the state according to a function"){
			runTransformer(List(1,2,3))(Transformer.Scan("")(_ + _)) should be(List("1", "12", "123"))
			runTransformer(List(1,2,3))(Transformer.Scan(0)(_ + _)) should be(List(1, 3, 6))
			runTransformer(List.empty[Int])(Transformer.Scan(0)(_ + _)) should be(Nil)
		}
		it("should catch errors thrown by the state advancement function and call handleError on the downstream handler"){
			runTransformer(List("1", "2", "hello"))(
				Transformer.Scan[Int, String](0)(_ + _.toInt).expandResults
			) should matchPattern {
				case List(Success(1), Success(3), Result.Error(err: NumberFormatException)) =>
			}
			runTransformer(List("hello", "1", "2"))(
				Transformer.Scan[Int, String](0)(_ + _.toInt).expandResults
			) should matchPattern {
				case List(Result.Error(err: NumberFormatException), Success(1), Success(3)) =>
			}
		}
		enforceIsFinishedContract(Transformer.Scan(0)(_ + _))
	}

	describe("Splitter.splitOnMatch"){
		def collectListsStartingWithOne(numbers: Int*) = {
			runTransformer(List(numbers: _*)){
				Splitter.splitOnMatch[Int](_ == 1) through Consumer.ToList[Int]
			}
		}

		it("should create a new substream when an input matches the predicate"){
			collectListsStartingWithOne(1,2,3,1,2,1,2,3,4,5) should be(List(
				List(1,2,3),
				List(1,2),
				List(1,2,3,4,5)
			))
		}

		it("should ignore prefix inputs if a substream hasn't started"){
			collectListsStartingWithOne(5,4,3,2,1,2,3,1,2,3) should be(List(
				List(1,2,3),
				List(1,2,3)
			))
		}

		it("should ignore all inputs if the predicate never matches"){
			collectListsStartingWithOne(2,3,4,5,6,7,8,9) should be(Nil)
		}

		it("should handle an immediate EOF without starting any substreams"){
			collectListsStartingWithOne() should be(Nil)
		}
	}
}
