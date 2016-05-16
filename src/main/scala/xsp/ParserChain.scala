package xsp

import javax.xml.stream.events.XMLEvent

import ChainSyntax.~
import xsp.handlers.CompoundHandler

class ParserChain[Context, P <: Chain[_, _], R, T](parserChain: P)
	(implicit handlerSetup: ParserChainSetup[Context, P, R], resultTupler: ResultChainTuple[R, T])
	extends Parser[Context, T] {
	def makeHandler(context: Context): Handler[XMLEvent, Result[T]] = makeHandler(Right(context))
	def makeHandler(error: Throwable): Handler[XMLEvent, Result[T]] = makeHandler(Left(error))

	private def makeHandler(contextOrError: Either[Throwable, Context]): Handler[XMLEvent, Result[T]] = {
		val innerHandlers = handlerSetup.createHandlersArray(parserChain, contextOrError)
		val reform = (handlerSetup.formResultChain _) andThen (resultTupler.toTuple _)
		new CompoundHandler(innerHandlers, reform)
	}
}

object ChainParserSyntax extends ChainParserSyntax
trait ChainParserSyntax {
	implicit class ParserChainTupled[P <: Chain[_, _]](parserChain: P) {
		def tupled[Context, R, T](implicit
			handlerSetup: ParserChainSetup[Context, P, R],
			resultTupler: ResultChainTuple[R, T]
		): Parser[Context, T] = new ParserChain(parserChain)
	}
}


trait ParserChainSetup[Context, P, R] {
	def length: Int
	def setupHandlers(
		parsers: P,
		context: Either[Throwable, Context],
		handlers: Array[Handler[XMLEvent, Result[Any]]]
	)
	def formResultChain(rawResults: IndexedSeq[Result[Any]]): R

	def createHandlersArray(
		parsers: P,
		context: Either[Throwable, Context]
	): IndexedSeq[Handler[XMLEvent, Result[Any]]] = {
		val handlers = new Array[Handler[XMLEvent, Result[Any]]](length)
		setupHandlers(parsers, context, handlers)
		handlers
	}
}

object ParserChainSetup {

	implicit def endCase[Context, A]: ParserChainSetup[Context, Parser[Context, A], Result[A]] = {
		new ParserChainEndSetup[Context, A]
	}
	implicit def linkCase[Context, PrefixP, PrefixR, A]
	(implicit prefixCase: ParserChainSetup[Context, PrefixP, PrefixR])
	: ParserChainSetup[Context, PrefixP ~ Parser[Context, A], PrefixR ~ Result[A]] = {
		new ParserChainLinkSetup[Context, PrefixP, PrefixR, A]
	}

	@inline private def makeSingleHandler[Ctx, T](
		parser: Parser[Ctx, T],
		context: Either[Throwable, Ctx]
	) = context match {
		case Right(ctx) => parser.makeHandler(ctx)
		case Left(error) => parser.makeHandler(error)
	}

	class ParserChainLinkSetup[Context, PrefixP, PrefixR, A]
	(implicit headSetup: ParserChainSetup[Context, PrefixP, PrefixR])
		extends ParserChainSetup[Context, PrefixP ~ Parser[Context, A], PrefixR ~ Result[A]] {

		val length = headSetup.length + 1
		def setupHandlers(
			parsers: PrefixP ~ Parser[Context, A],
			context: Either[Throwable, Context],
			handlers: Array[Handler[XMLEvent, Result[Any]]]
		): Unit = {
			val tailParser = parsers.tail
			val headChain = parsers.head
			val handler = makeSingleHandler(tailParser, context)
			handlers(length - 1) = handler
			headSetup.setupHandlers(headChain, context, handlers)
		}
		def formResultChain(rawResults: IndexedSeq[Result[Any]]): PrefixR ~ Result[A] = {
			new Chain(headSetup.formResultChain(rawResults), rawResults(length - 1).asInstanceOf[Result[A]])
		}
	}

	class ParserChainEndSetup[Context, A]
		extends ParserChainSetup[Context, Parser[Context, A], Result[A]] {

		val length = 1
		def setupHandlers(
			parser: Parser[Context, A],
			context: Either[Throwable, Context],
			handlers: Array[Handler[XMLEvent, Result[Any]]]
		): Unit = {
			handlers(0) = makeSingleHandler(parser, context)
		}
		def formResultChain(rawResults: IndexedSeq[Result[Any]]): Result[A] = {
			rawResults(0).asInstanceOf[Result[A]]
		}
	}

}