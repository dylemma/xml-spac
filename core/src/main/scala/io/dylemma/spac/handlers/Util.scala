package io.dylemma.spac.handlers

import io.dylemma.spac.{Handler, HandlerFactory}

import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

object Util {

	/** Helper function for Splitter implementations; creates a Handler given a matched context
	  * and a joiner function. Exceptions from the matched context or from calling the joiner
	  * function will be caught, causing the generated Handler to be a `OneShotHandler(Failure(wrappedErr))`.
	  */
	def initHandler[Ctx, In, Out](matchedContext: Try[Ctx], joiner: Ctx => HandlerFactory[In, Try[Out]]): Handler[In, Try[Out]] = {
		matchedContext match {
			case Success(ctx) =>
				try joiner(ctx).makeHandler() catch { case NonFatal(joinErr) =>
					val wrappedErr = new Exception(s"failed to create a handler from context $ctx", joinErr)
					new OneShotHandler(Failure(wrappedErr))
				}
			case Failure(err) =>
				val wrappedErr = new Exception("failed to match a context", err)
				new OneShotHandler(Failure(wrappedErr))
		}
	}

}
