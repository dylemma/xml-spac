package io.dylemma.spac
package impl

import cats.data.Chain

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class ParserOrElseChain[In, Out] private(chain: Chain[Parser[In, Out]]) extends Parser[In, Out] {
	def newHandler = {
		val buffer = chain.iterator.zipWithIndex.map { case (p, i) => i -> p.newHandler }.toArray
		new ParserOrElseChain.Handler(buffer, buffer.length, Nil)
	}
}

object ParserOrElseChain {
	def apply[In, Out](members: Chain[Parser[In, Out]]): ParserOrElseChain[In, Out] = {
		val chain = members.flatMap {
			case ParserOrElseChain(innerChain) => innerChain
			case p => Chain.one(p)
		}
		new ParserOrElseChain(chain)
	}

	class Handler[In, Out](
		buffer: Array[(Int, Parser.Handler[In, Out])],
		var numActive: Int,
		var errors: List[(Int, Throwable)]
	) extends Parser.Handler[In, Out]
	{
		def step(in: In) = {
			@tailrec def loop(i: Int): Option[Out] = {
				if (i < numActive) {
					val (j, p) = buffer(i)
					Try { p.step(in) } match {
						case Success(Right(cont)) =>
							buffer(i) = j -> cont
							loop(i + 1)
						case Failure(e) =>
							errors ::= (j, e)
							ArrayHelper.placeAndShiftLeft(buffer, i, numActive - 1)
							numActive -= 1
							loop(i) // the next value to inspect just got moved into this index, so no +1 here
						case Success(Left(out)) =>
							Some(out)
					}
				} else {
					None
				}
			}
			loop(0) match {
				case None =>
					if (numActive == 0) {
						// done, where all of the parsers failed
						val sortedErrors = errors.sortBy(_._1).map(_._2)
						throw new SpacException.FallbackChainFailure(sortedErrors)
					} else {
						// some parsers still running
						Right(this)
					}
				case Some(out) =>
					// finished with a result
					Left(out)
			}


		}
		def finish() = {
			@tailrec def loop(i: Int): Option[Out] = {
				if (i < numActive) {
					val (j, p) = buffer(i)
					Try { p.finish() } match {
						case Success(out) => Some(out)
						case Failure(err) =>
							errors ::= (j, err)
							ArrayHelper.placeAndShiftLeft(buffer, i, numActive - 1)
							numActive -= 1
							loop(i)
					}
				} else {
					None
				}
			}

			loop(0) match {
				case Some(out) => out
				case None =>
					// all underlying parsers failed; collect their errors in order of their original index
					val sortedErrors = errors.sortBy(_._1).map(_._2)
					throw new SpacException.FallbackChainFailure(sortedErrors)
			}
		}
	}
}
