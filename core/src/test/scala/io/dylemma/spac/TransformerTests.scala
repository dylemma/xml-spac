package io.dylemma.spac

import io.dylemma.spac.handlers.{ConstantHandler, ManualFinish}
import org.scalatest.{FunSpec, Matchers}

import scala.util.{Failure, Success}
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
			val result = Transformer.Map{s: String => s.toInt} >> Consumer.ToList[Int].wrapSafe consume List("not a number")
			result should matchPattern {
				case Failure(err: NumberFormatException) =>
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
			val result = Transformer.Collect[String, Int]{ case s => s.toInt } >> Consumer.ToList[Int].wrapSafe consume List("1", "2", "hi")
			result should matchPattern { case Failure(e: NumberFormatException) => }
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
				Transformer.Scan[Int, String](0)(_ + _.toInt).wrapSafe
			) should matchPattern {
				case List(Success(1), Success(3), Failure(err: NumberFormatException)) =>
			}
			runTransformer(List("hello", "1", "2"))(
				Transformer.Scan[Int, String](0)(_ + _.toInt).wrapSafe
			) should matchPattern {
				case List(Failure(err: NumberFormatException), Success(1), Success(3)) =>
			}
		}
		enforceIsFinishedContract(Transformer.Scan(0)(_ + _))
	}

	describe("Splitter.splitOnMatch"){
		def collectListsStartingWithOne(numbers: Int*) = {
			runTransformer(List(numbers: _*)){
				Splitter.splitOnMatch[Int](_ == 1) through Consumer.ToList[Int].wrapSafe
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

	describe("Transformer.FunnelAll"){
		val nums = List(1,2,3,4,5,6)
		val multiplesOfThree = Transformer.Collect[Int, String]{ case i if i%3 == 0 => s"$i/3" }
		val multiplesOfTwo = Transformer.Collect[Int, String]{ case i if i%2 == 0 => s"$i/2" }

		enforceIsFinishedContract(multiplesOfThree funnel multiplesOfTwo)

		it("should funnel results of each transformer to the downstream consumer"){
			val funnel = Transformer.FunnelAll(multiplesOfTwo :: multiplesOfThree :: Nil).consumeToList
			funnel.consume(nums) should be(
				List("2/2", "3/3", "4/2", "6/2", "6/3")
			)
		}

		it("should send results in the order of the transformers in the funnel"){
			val funnel = Transformer.FunnelAll(multiplesOfThree :: multiplesOfTwo :: Nil).consumeToList
			funnel.consume(nums) should be(
				List("2/2", "3/3", "4/2", "6/3", "6/2")
			)
		}

		it("should exit early if the downstream emits a result"){
			var leftCount = 0
			var rightCount = 0
			val leftTransform = Transformer.SideEffect[Int](_ => leftCount += 1)
			val rightTransform = Transformer.SideEffect[Int](_ => rightCount += 1)
			val funnel = leftTransform.funnel(rightTransform) >> Consumer.Constant("hi")
			// early-exit thanks to "Constant" consumer.
			// The left transformer would have handled one input just to activate the consumer,
			// which would have returned a result, causing the early exit, ensuring the "right"
			// transformer's handler never runs.
			funnel.consume(nums) should be("hi")
			leftCount should be(1)
			rightCount should be(0)
		}

		it("should exit early if all of the funnel handlers finish"){
			val take1 = Transformer.Take[Int](1)
			val take2 = Transformer.Take[Int](2)
			val funnel = (take1 funnel take2).consumeToList.makeHandler()

			// manually drive the consumption of a data source, so we can test exactly when the "exit" condition is hit
			funnel.handleInput(1) should be(None)
			funnel.handleInput(2) should be(Some(List(1, 1, 2)))
		}

		it("should continue if the downstream and at least one funnel is unfinished"){
			val take1 = Transformer.Take[Int](1)
			val passThrough = Transformer.SideEffect[Int](_ => ())
			val funnel = (take1 funnel passThrough).consumeToList
			funnel.consume(nums) should be(List(1, 1, 2, 3, 4, 5, 6))
		}
	}
}
