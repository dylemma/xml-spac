package io.dylemma.spac
package impl

import cats.data.NonEmptyChain

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class TransformerStack[In, Out](stack: NonEmptyChain[Transformer[Any, Any]]) extends Transformer[In, Out] {
	def newHandler = {
		val members = stack.toChain.iterator.map(_.newHandler).toArray
		new TransformerStack.Handler(members)
	}
}

object TransformerStack {
	class Handler[In, Out](members: Array[Transformer.Handler[Any, Any]]) extends Transformer.Handler[In, Out] {

		def step(in: In) = {
			@tailrec def stepFrom(i: Int, incoming: Emit[Any], isFinished: Boolean): (Emit[Out], Option[Transformer.Handler[In, Out]]) = {
				if (i < members.length) {
					val h = members(i)
					Try {
						h.stepMany(incoming) match {
							case (outs, Left(leftovers)) =>
								(outs, true)
							case (outs, Right(cont)) =>
								members(i) = cont
								if (isFinished) (outs ++ cont.finish(), true)
								else (outs, false)
						}
					} match {
						case Success((out, hasFinished)) =>
							stepFrom(i + 1, out, hasFinished)
						case Failure(e) =>
							throw unwindFrom(i, e)
					}
				} else {
					val cont = if (isFinished) None else Some(this)
					incoming.asInstanceOf[Emit[Out]] -> cont
				}
			}
			stepFrom(0, Emit.one(in), isFinished = false)
		}

		def finish() = {
			@tailrec def finishFrom(i: Int, toEmit: Emit[Any]): Emit[Out] = {
				if (i < members.length) {
					val h = members(i)
					Try {
						h.stepMany(toEmit) match {
							case (outs, Left(leftovers)) => outs
							case (outs, Right(cont)) => outs ++ cont.finish()
						}
					} match {
						case Success(nextEmit) => finishFrom(i+1, nextEmit)
						case Failure(err) => throw unwindFrom(i,  err)
					}
				} else {
					// no more handlers, whatever we have to emit is the final handler's output of type Out
					toEmit.asInstanceOf[Emit[Out]]
				}
			}
			finishFrom(0, Emit.nil)
		}
		@tailrec private def unwindFrom(i: Int, err: Throwable): Throwable = {
			if (i < 0) {
				err
			} else {
				val h = members(i)
				val err2 = h.unwind(err)
				unwindFrom(i - 1, err2)
			}
		}
		override def unwind(err: Throwable) = {
			unwindFrom(members.length - 1, err)
		}
	}
}
