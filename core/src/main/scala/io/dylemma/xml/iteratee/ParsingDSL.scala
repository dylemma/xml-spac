package io.dylemma.xml.iteratee

import javax.xml.namespace.QName
import javax.xml.stream.XMLStreamException
import javax.xml.stream.events.XMLEvent
import scala.concurrent.ExecutionContext
import scala.language.implicitConversions

import io.dylemma.xml.MatchResult._
import io.dylemma.xml.{ MatchResultSimplifier, MatchResult, MatcherSemantics }
import io.dylemma.xml.event._
import io.dylemma.xml.iteratee.IterateeHelpers._
import play.api.libs.functional.{ FunctionalBuilderOps, FunctionalCanBuild, Functor, ~ }
import play.api.libs.iteratee.{ Enumeratee, Iteratee }

/**
 * Created by dylan on 10/10/2015.
 */
object ParsingDSL extends MatcherSemantics[OpenTag] {

	// re-export the Parser type and companion object to save clients an import
	val Parser = io.dylemma.xml.iteratee.Parser
	type Parser[T] = io.dylemma.xml.iteratee.Parser[T]
	import Parser._

	/*
	Eventually want to support wildcard paths e.g.
	__ \ foo \ * \ bar
	__ \ foo \ ** \ baz
	__ \ ** \ baz
	__ \ foo \ **
	 */

	class ContextMatcherOps[C](matchContext: TagStack => Option[C]) {

		// make parsers that handle text from matched elements
		def asTextParserFactory: ParserCreator[String] = PreTextParser(matchContext)
		def text: Parser[String] = asTextParserFactory.parseConsume()
		def textOpt: Parser[Option[String]] = asTextParserFactory.parseOptional
		def textList: Parser[List[String]] = asTextParserFactory.parseList
		def %(t: Text.type) = text
		def %(t: Text.asOption.type) = textOpt
		def %(t: Text.asList.type) = textList

		// make parsers that handle attributes from matched elements
		def attr(attribute: String): Parser[String] = MandatoryAttributeParser(matchContext, attribute).parseSingle
		def attrOpt(attribute: String): Parser[Option[String]] = OptionalAttributeParser(matchContext, attribute).parseSingle
		def %(attribute: String) = attr(attribute)
		def %?(attribute: String) = attrOpt(attribute)

		// make parsers that build the matched elements into new values
		def asParserFactory[T: Parser]: ParserCreator[T] = DelegateParser[C, T](matchContext, implicitly)
		def as[T: Parser]: Parser[T] = asParserFactory[T].parseSingle
		def asOptional[T: Parser]: Parser[Option[T]] = asParserFactory[T].parseOptional
		def asList[T: Parser]: Parser[List[T]] = asParserFactory[T].parseList

		// make parsers that just do side-effects when a result passes through
		def foreach[T: Parser](f: T => Unit)(implicit ec: ExecutionContext): Parser[Unit] =
			asParserFactory[T].parseSideEffect(Iteratee.foreach[Result[T]]{ _.foreach(f) })
		def foreachResult[T: Parser](f: Result[T] => Unit)(implicit ec: ExecutionContext): Parser[Unit] =
			asParserFactory[T].parseSideEffect(Iteratee.foreach[Result[T]](f))

		// provide an Iteratee to consume events within the matched context and produce values
		def produce[To](innerConsumer: Iteratee[XMLEvent, To])(implicit ec: ExecutionContext): Enumeratee[XMLEvent, Option[To]] = {
			subdivideXml(matchContext).combineWith(innerConsumer)
		}
	}

	implicit def makeContextMatcherOps[M <: MatchResult[_], C](matcher: ListMatcher[M])(
		implicit simplifier: MatchResultSimplifier[M, C]
	): ContextMatcherOps[C] = new ContextMatcherOps({ stack =>
		for{ (r, _) <- matcher(stack) } yield simplifier.simplify(r)
	})

	implicit def makeContextMatcherOps[M <: MatchResult[_], C](matcher: Matcher[M])(
		implicit simplifier: MatchResultSimplifier[M, C]
	): ContextMatcherOps[C] = makeContextMatcherOps(matcher.asListMatcher)

//	implicit def makeContextMatcherOps[M, R <: MatchResult[_], C](matcher: M)(
//		implicit toListMatcher: M => ListMatcher[R], simplifier: MatchResultSimplifier[R, C]
//	): ContextMatcherOps[C] = new ContextMatcherOps({ stack =>
//		for{ (r, _) <- toListMatcher(matcher)(stack) } yield simplifier.simplify(r)
//	})

	object Text {
		object asList
		object asOption
	}

//	implicit class MatcherParserOps[M <: MatchResult[_], C](matcher: Matcher[M])(implicit simplifier: MatchResultSimplifier[M, C])
//		extends ListMatcherParserOps(matcher.asListMatcher)

//	trait OldPathSpecification[C] {
//		def pathSpec(tagStack: TagStack): Option[C]
//
//		def \(text: Text.type): Parser[String] = new PreTextParser(pathSpec).parseConsume()
//		def \(text: Text.asOption.type): Parser[Option[String]] = new PreTextParser(pathSpec).parseOptional
//		def \(text: Text.asList.type): Parser[List[String]] = new PreTextParser(pathSpec).parseList
//		def consumeText(consumer: Iteratee[Result[String], Unit]) = new PreTextParser(pathSpec).parseSideEffect(consumer)
//
//		def %(attribute: String): Parser[String] = MandatoryAttributeParser(pathSpec, attribute).parseSingle
//		def %?(attribute: String): Parser[Option[String]] = OptionalAttributeParser(pathSpec, attribute).parseSingle
//
//		def as[T: Parser] = DelegateParser[C, T](pathSpec, implicitly).parseSingle
//		def asOptional[T: Parser] = DelegateParser[C, T](pathSpec, implicitly).parseOptional
//		def asList[T: Parser] = DelegateParser[C, T](pathSpec, implicitly).parseList
//		def consumeAs[T: Parser](consumer: Iteratee[Result[T], Unit]) = DelegateParser[C, T](pathSpec, implicitly).parseSideEffect(consumer)
//
//		def asEnumeratee[To](consumer: Iteratee[XMLEvent, To])(implicit ec: ExecutionContext): Enumeratee[XMLEvent, Option[To]] = {
//			subdivideXml(pathSpec).combineWith(consumer)
//		}
//	}

	/** Base context matcher that will match all contexts without
		* actually consuming any of the tag stack
		*/
	object Root extends ListMatcher[Ok.type] {
		def apply(inputs: List[OpenTag]) = Some(Ok -> inputs)
	}
	/** Tag matcher that always matches
		*/
	val * = Matcher.predicate{ _ => true }

	def tag(qname: QName) = Matcher.predicate{ _.name == qname }
	def tag(name: String) = Matcher.predicate { _.name.getLocalPart == name }
	implicit def stringToTagMatcher(name: String): Matcher[Ok.type] = tag(name)

	def attr(qname: QName) = Matcher{ _.attrs get qname }
	def attr(name: String): Matcher[SingleValue[String]] = attr(new QName(name))

//	implicit def upgradeSegmentToPathMatcher(segment: SegmentMatch): PathMatcherBySegments = {
//		PathMatcherBySegments(segment :: Nil)
//	}
//
//	implicit def stringToSegmentMatcher(s: String): SegmentMatch = ExactSegmentMatch(s)
//
//	case class PathMatcherBySegments(segments: List[SegmentMatch]) extends PathSpecification[Unit]{
//
//		def apply(stack: TagStack) = cmp(segments, stack)
//		def \(segment: SegmentMatch) = PathMatcherBySegments(segments :+ segment)
//
//		protected def cmp(segs: List[SegmentMatch], stack: TagStack): Option[Unit] = (segs, stack) match {
//			case (Nil, s) => Some(()) // reached the end of the required path; we're in match zone
//			case (list, Nil) => None // reached the end of the path but still have more required
//			case (headSeg :: tailSegs, headTag :: tailTags) =>
//				if(headSeg matches headTag) cmp(tailSegs, tailTags) // current segment matches; recurse
//				else None // mismatch
//		}
//	}

	/** This object allows a Parser to be combined with other parsers via
		* `and` or `~` when you import `play.api.libs.functional.syntax._`
		*/
	implicit object FunctionCanBuildParser extends FunctionalCanBuild[Parser] {
		def apply[A, B](pa: Parser[A], pb: Parser[B]) = new Parser[A ~ B]{
			def toIteratee(implicit ec: ExecutionContext): Iteratee[XMLEvent, Result[A ~ B]] = {
				Enumeratee.zipWith(pa.toIteratee, pb.toIteratee) { (ra, rb) =>
					for (a <- ra; b <- rb) yield new ~(a, b)
				}
			}
		}
	}

	implicit object FunctorForParser extends Functor[Parser] {
		def fmap[A, B](m: Parser[A], f: A => B): Parser[B] = new Parser[B] {
			def toIteratee(implicit ec: ExecutionContext) = {
				m.toIteratee.map{ result => result.map(f) }
			}
		}
	}

	/** This is essentially a shortcut for `play.api.libs.functional.syntax.toParserBuilderOps` for Parsers. */
	implicit def toParserBuilderOps[A](parser: Parser[A]) = new FunctionalBuilderOps[Parser, A](parser)

	def getOrElseEmpty[T](implicit ec: ExecutionContext) = Enumeratee.map[Option[Result[T]]]{
		case None => Empty
		case Some(result) => result
	}

	type PathSpecification[C] = TagStack => Option[C]

	case class OptionalAttributeParser[C](pathSpec: PathSpecification[C], attribute: String) extends ParserCreator[Option[String]] {
		def toEnumeratee(implicit ec: ExecutionContext) = {
			subdivideXml(pathSpec).combineWith[Result[Option[String]]] {
				val lookupAttr = Enumeratee.collect[XMLEvent] { case StartElement(_, attrs) => Success(attrs get attribute) }
				lookupAttr &>> Iteratee.head.map {
					// if the *head* was None, it means we never even encountered an element, so give an `Empty` result
					case None => Empty
					// otherwise, some element was encountered, and the result is whatever optional attribute value was there
					case Some(result) => result
				}
			} ><> getOrElseEmpty
		}
	}

	case class MandatoryAttributeParser[C](pathSpec: PathSpecification[C], attribute: String) extends ParserCreator[String] {
		def toEnumeratee(implicit ec: ExecutionContext) = {
			subdivideXml(pathSpec).combineWith[Result[String]] {
				val lookupAttr = Enumeratee.collect[XMLEvent]{
					case e @ StartElement(_, attrs) => attrs.get(attribute) match {
						case None =>
							val msg = s"Expected a value for the '$attribute' attribute, but none was found"
							Error(new XMLStreamException(msg, e.getLocation))
						case Some(value) =>
							Success(value)
					}
				}
				// if no elements were found, return Empty rather than an error
				lookupAttr &>> Iteratee.head.map { _ getOrElse Empty }
			} ><> getOrElseEmpty
		}
	}

	case class PreTextParser[C](pathSpec: PathSpecification[C]) extends ParserCreator[String] {
		def toEnumeratee(implicit ec: ExecutionContext) = subdivideXml(pathSpec).combineWith{
			val collectText = Enumeratee.collect[XMLEvent] { case Characters(text) => text.trim }
			val consumeTextAsSuccess = Iteratee.consume[String]().map[Result[String]](Success(_))
			collectText &>> consumeTextAsSuccess
		} ><> getOrElseEmpty[String]
	}

	case class DelegateParser[C, T](pathSpec: PathSpecification[C], innerParser: Parser[T]) extends ParserCreator[T] {
		def toEnumeratee(implicit ec: ExecutionContext) = {
			subdivideXml(pathSpec).combineWith(innerParser.toIteratee) ><> getOrElseEmpty
		}
	}

}
