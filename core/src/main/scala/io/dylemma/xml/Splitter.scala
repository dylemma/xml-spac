package io.dylemma.xml

import scala.concurrent.ExecutionContext

import io.dylemma.xml.IterateeHelpers.OpenTag
import io.dylemma.xml.Result.{ Error, Empty }
import play.api.libs.iteratee.{ Iteratee, Enumeratee }

trait Splitter[+In] { self =>
	def through[Out](innerParser: ParserBase[In, Out]): Transformer[Out]

	def mapContext[In2](f: In => In2): Splitter[In2] = new Splitter[In2] {
		def through[Out](innerParser: ParserBase[In2, Out]): Transformer[Out] = {
			self.through(innerParser.unmapContext(f))
		}
	}

	@inline def as[Out](implicit innerParser: ParserBase[In, Out]): Parser[Out] = through(innerParser).parseSingle
	@inline def asOptional[Out](implicit innerParser: ParserBase[In, Out]): Parser[Option[Out]] = through(innerParser).parseOptional
	@inline def asList[Out](implicit innerParser: ParserBase[In, Out]): Parser[List[Out]] = through(innerParser).parseList
	@inline def foreach[Out](thunk: Out => Unit)(implicit innerParser: ParserBase[In, Out]): Parser[Unit] = through(innerParser).foreach(thunk)
	@inline def foreachResult[Out](thunk: Result[Out] => Unit)(implicit innerParser: ParserBase[In, Out]): Parser[Unit] = through(innerParser).foreachResult(thunk)

	@inline def attr(attribute: String) = through(Parser.parseMandatoryAttribute(attribute)).parseSingle
	@inline def %(attribute: String) = attr(attribute)

	@inline def attrOpt(attribute: String) = through(Parser.parseOptionalAttribute(attribute)).parseSingle
	@inline def %?(attribute: String) = attrOpt(attribute)

	@inline def text = through(Parser.parseText)
	@inline def textConcat = text.parseConcat()
	@inline def textOption = text.parseOptional
	@inline def textList = text.parseList
}

/** Splitter implementation based on a 'tag stack' context matcher function.
	* As the stream goes by, a 'tag stack' will be maintained; the `matchContext`
	* function will be called on that stack for each event to identify a possible
	* context. Substreams are series of consecutive events that match a context.
	*
	* @tparam In The identified context's type
	*/
trait ContextMatchSplitter[+In] extends Splitter[In] {

	def matchContext(tagStack: List[OpenTag]): Result[In]

	def through[Out](innerParser: ParserBase[In, Out]): Transformer[Out] = {
		new Transformer[Out] {
			def toEnumeratee(implicit ec: ExecutionContext) = IterateeHelpers
				.subdivideXml(matchContext)
				.combineWith{
					case scala.util.Success(in) => innerParser.toIteratee(in)
					case scala.util.Failure(err) => Iteratee.skipToEof.map(_ => Error(err))
				}
				.compose(Enumeratee.map(_ getOrElse Empty))
		}
	}
}
