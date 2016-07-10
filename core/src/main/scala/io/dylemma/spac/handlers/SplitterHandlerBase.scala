package io.dylemma.spac.handlers

import io.dylemma.spac.{Handler, Result, debug}

import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

/**
	* Created by dylan on 7/9/2016.
	*/
trait SplitterHandlerBase[In, Context, P, Out] extends Handler[In, Out] {

	protected var currentParserHandler: Option[Handler[In, Try[P]]] = None
	protected def downstream: Handler[P, Out]
	protected def debugName: String

	def isFinished = downstream.isFinished

	def handleEnd(): Out = {
		feedEndToCurrentParser()
		  .map(debug as "Got inner parser result (from EOF)")
		  .flatMap(feedResultToDownstream)
		  .getOrElse{ downstream.handleEnd() }
	}

	def handleError(err: Throwable): Option[Out] = {
		feedErrorToCurrentParser(err)
		  .map(debug as "Got inner parser result (from error)")
		  .flatMap(feedResultToDownstream)
	}

	protected def feedEndToCurrentParser(): Option[Try[P]] = {
		for {
			handler <- currentParserHandler
			if !handler.isFinished
		} yield {
			currentParserHandler = None
			try handler.handleEnd() catch { case NonFatal(err) =>
				throw new Exception(
					s"Error in inner parser-handler [$handler] while running $debugName at handleEOF",
					err
				)
			}
		}
	}

	protected def feedEventToCurrentParser(input: In): Option[Try[P]] = {
		for {
			handler <- currentParserHandler
			if !handler.isFinished
			result <- try handler.handleInput(input) catch { case NonFatal(err) =>
				throw new Exception(
					s"Error in inner parser-handler [$handler] while running $debugName at handleInput [$input]",
					err
				)
			}
		} yield {
			currentParserHandler = None
			result
		}
	}

	protected def feedErrorToCurrentParser(err: Throwable): Option[Try[P]] = {
		for {
			handler <- currentParserHandler
			if !handler.isFinished
			result <- try handler.handleError(err) catch { case NonFatal(err2) =>
				throw new Exception(
					s"Error in inner parser-handler [$handler] while running $debugName at handleError [$err]",
					err2
				)
			}
		} yield {
			currentParserHandler = None
			result
		}
	}

	protected def feedResultToDownstream(result: Try[P]): Option[Out] = {
		try {
			if (downstream.isFinished) None
			else result match {
				case Success(p) => downstream.handleInput(p)
				case Failure(err) => downstream.handleError(err)
			}
		} catch {
			case NonFatal(err) => throw new Exception(
				s"Error passing [$result] to downstream handler [$downstream] while running $debugName",
				err
			)
		}
	}
}
