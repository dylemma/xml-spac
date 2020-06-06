package io.dylemma.spac.old.handlers

import io.dylemma.spac.old.{Handler, Parser}

import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

object Util {

	/** Helper function for Splitter implementations; creates a Handler given a matched context
	  * and a joiner function. Exceptions from the matched context or from calling the joiner
	  * function will be caught, causing the generated Handler to be a `OneShotHandler(Failure(wrappedErr))`.
	  */
	def initHandler[Ctx, In, Out](matchedContext: Try[Ctx], joiner: Ctx => Parser[In, Out]): Handler[In, Out] = {
		matchedContext match {
			case Success(ctx) =>
				try joiner(ctx).makeHandler() catch { case NonFatal(joinErr) =>
					throw new Exception(s"failed to create a handler from context $ctx", joinErr)
				}
			case Failure(err) =>
				throw new Exception("failed to match a context", err)
		}
	}

}
