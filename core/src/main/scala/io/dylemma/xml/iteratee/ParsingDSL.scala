package io.dylemma.xml.iteratee

import javax.xml.stream.XMLStreamException
import javax.xml.stream.events.XMLEvent
import scala.concurrent.ExecutionContext
import scala.language.implicitConversions

import io.dylemma.xml.event._
import io.dylemma.xml.iteratee.IterateeHelpers._
import io.dylemma.xml.iteratee.PathMatching.{ ExactSegmentMatch, WildcardSegmentMatch, SegmentMatch }
import play.api.libs.functional.{ FunctionalBuilderOps, FunctionalCanBuild, Functor, ~ }
import play.api.libs.iteratee.{ Enumeratee, Iteratee }

/**
 * Created by dylan on 10/10/2015.
 */
object ParsingDSL {

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

	trait PathSpecification[C] {
		def matchContext(tagStack: TagStack): Option[C]

		def \(text: Text.type): Parser[String] = new PreTextParser(this).parseConsume()
		def \(text: Text.asOption.type): Parser[Option[String]] = new PreTextParser(this).parseOptional
		def \(text: Text.asList.type): Parser[List[String]] = new PreTextParser(this).parseList
		def consumeText(consumer: Iteratee[Result[String], Unit]) = new PreTextParser(this).parseSideEffect(consumer)

		def %(attribute: String): Parser[String] = MandatoryAttributeParser(this, attribute).parseSingle
		def %?(attribute: String): Parser[Option[String]] = OptionalAttributeParser(this, attribute).parseSingle

		def as[T: Parser] = DelegateParser[C, T](this, implicitly).parseSingle
		def asOptional[T: Parser] = DelegateParser[C, T](this, implicitly).parseOptional
		def asList[T: Parser] = DelegateParser[C, T](this, implicitly).parseList
		def consumeAs[T: Parser](consumer: Iteratee[Result[T], Unit]) = DelegateParser[C, T](this, implicitly).parseSideEffect(consumer)

		def asEnumeratee[To](consumer: Iteratee[XMLEvent, To])(implicit ec: ExecutionContext): Enumeratee[XMLEvent, Option[To]] = {
			subdivideXml(matchContext).combineWith(consumer)
		}
	}

	// See PathSpecification's `def \(Text)` and `def \(Text.asList)`
	object Text {
		object asList
		object asOption
	}

	object Root extends PathMatcherBySegments(Nil)
	val * = WildcardSegmentMatch

	implicit def upgradeSegmentToPathMatcher(segment: SegmentMatch): PathMatcherBySegments = {
		PathMatcherBySegments(segment :: Nil)
	}

	implicit def stringToSegmentMatcher(s: String): SegmentMatch = ExactSegmentMatch(s)

	case class PathMatcherBySegments(segments: List[SegmentMatch]) extends PathSpecification[Unit]{

		def matchContext(stack: TagStack) = cmp(segments, stack)
		def \(segment: SegmentMatch) = PathMatcherBySegments(segments :+ segment)

		protected def cmp(segs: List[SegmentMatch], stack: TagStack): Option[Unit] = (segs, stack) match {
			case (Nil, s) => Some(()) // reached the end of the required path; we're in match zone
			case (list, Nil) => None // reached the end of the path but still have more required
			case (headSeg :: tailSegs, headTag :: tailTags) =>
				if(headSeg matches headTag) cmp(tailSegs, tailTags) // current segment matches; recurse
				else None // mismatch
		}
	}

	case class XPathContextMatcher(path: List[String]) extends PathSpecification[TagStack]{
		def matchContext(stack: TagStack) = cmp(path, stack)
		def \(segment: String) = XPathContextMatcher(path :+ segment)

		protected def cmp(p: List[String], s: TagStack): Option[TagStack] = (p, s) match {
			case (Nil, s) => Some(s) // reached the end of the path without failure; match!
			case (list, Nil) => None // there is more path, but no more stack
			case (ph :: pt, OpenTag(Name(sh), _) :: st) =>
				if(ph == sh) cmp(pt, st) // current segment matches; check remaining segments
				else None // mismatch
		}
	}

	/** This object allows a Parser to be combined with other parsers via
		* `and` or `~` when you import `play.api.libs.functional.syntax._`
		*/
	implicit object FunctionCanBuildParser extends FunctionalCanBuild[Parser] {
		def apply[A, B](pa: Parser[A], pb: Parser[B]) = new Parser[A ~ B]{
			def toIteratee(implicit ec: ExecutionContext): Iteratee[XMLEvent, Result[A ~ B]] = {
				Enumeratee.zipWith(pa.toIteratee, pb.toIteratee) { (ra, rb) =>
					import play.api.libs.functional.{ ~ => Tilde }
					//println(s"Combine $ra + $rb")
					for (a <- ra; b <- rb) yield Tilde(a, b)
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

	case class OptionalAttributeParser[C](pathSpec: PathSpecification[C], attribute: String) extends ParserCreator[Option[String]] {
		def toEnumeratee(implicit ec: ExecutionContext) = {
			subdivideXml(pathSpec.matchContext).combineWith[Result[Option[String]]] {
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
			subdivideXml(pathSpec.matchContext).combineWith[Result[String]] {
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

	class PreTextParser[C](pathSpec: PathSpecification[C]) extends ParserCreator[String] {
		def toEnumeratee(implicit ec: ExecutionContext) = subdivideXml(pathSpec.matchContext).combineWith{
			val collectText = Enumeratee.collect[XMLEvent] { case Characters(text) => text.trim }
			val consumeTextAsSuccess = Iteratee.consume[String]().map[Result[String]](Success(_))
			collectText &>> consumeTextAsSuccess
		} ><> getOrElseEmpty[String]
	}

	case class DelegateParser[C, T](pathSpec: PathSpecification[C], innerParser: Parser[T]) extends ParserCreator[T] {
		def toEnumeratee(implicit ec: ExecutionContext) = {
			subdivideXml(pathSpec.matchContext).combineWith(innerParser.toIteratee) ><> getOrElseEmpty
		}
	}

}
