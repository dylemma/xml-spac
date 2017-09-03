package io.dylemma.spac

import javax.xml.namespace.QName
import javax.xml.stream.events.XMLEvent

import io.dylemma.spac.handlers._

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

/** A HandlerFactory specialized for XMLEvents, which produces results wrapped in a `Try`.
  * Parsers can be combined and run to process complex XML structures.
  *
  * @tparam Out Result type
  *
  * @define hf parser
  * @define HF `Parser`
  */
trait Parser[+Out] extends AbstractHandlerFactory[XMLEvent, Out, Try, ({ type H[-in,+out] = Parser[out] })#H] { self =>

	def mapResult[B](f: Try[Out] => Try[B]): Parser[B] = new Parser[B] {
		def makeHandler() = new MappedConsumerHandler(f, self.makeHandler())
		override def toString = s"$self >> Map($f)"
	}

	def and[O2](p2: Parser[O2]) = new ParserCombination.Combined2(self, p2)
	def ~[O2](p2: Parser[O2]) = new ParserCombination.Combined2(self, p2)

	/** An intermediate object with an `apply` and `flatMap` that both create a sequenced Parser
	  * which combines this Parser with a function to create the next one.
	  *
	  * Examples:
	  * {{{
	  *    val p1: Parser[A] = /* ... */
	  *    def getP2(p1Result: A): Parser[B] = /* ... */
	  *    val combined: Parser[B] = p1.followedBy(getP2)
	  *
	  *    // alternative `flatMap` syntax
	  *    val combined: Parser[B] = for {
	  *      p1Result <- p1.followedBy
	  *      p2Result <- getP2(p1Result)
	  *    } yield p2Result
	  * }}}
	  *
	  * An example of where this is useful is when a parser for XML element depends on values
	  * parsed from one of its previous siblings, but where you don't want to wait until the
	  * end of their parent element before they can be combined.
	  *
	  * @return An intermediate object which has an `apply` and `flatMap` that can be used
	  *         to combine this Parser and another in a sequence.
	  */
	object followedBy extends FollowedBy[Parser, Out] {
		def apply[T2](getNext: Out => Parser[T2]): Parser[T2] = new Parser[T2] {
			override def toString = s"$self.followedBy($getNext)"
			def makeHandler(): Handler[XMLEvent, Try[T2]] = {
				val handler1 = self.makeHandler()
				def getHandler2(h1Result: Try[Out]) = h1Result match {
					case Success(result) => getNext(result).makeHandler()
					case Failure(err) => new ConstantHandler(Failure(err))
				}
				new SequencedInStackHandler(handler1, getHandler2)
			}
		}
	}

	/** An intermediate object that can be used to create a Transformer from result of this Parser.
	  *
	  * Examples:
	  * {{{
	  *    val p1: Parser[A] = /* ... */
	  *    def getP2Stream(p1Result: A): Transformer[XMLEvent, B] = /* ... */
	  *    val combined: Transformer[XMLEvent, B] = p1.andThenStream(getP2Stream)
	  *
	  *    // alternative `flatMap` syntax
	  *    val combined: Transformer[XMLEvent, B] = for {
	  *      p1Result <- p1.andThenStream
	  *      p2Result <- getP2Stream(p1Result)
	  *    } yield p2Result
	  * }}}
	  *
	  * An example of where this is useful is when an XML element contains some "dictionary" object
	  * at the beginning, followed by a sequence of "data" objects which reference the dictionary.
	  * For large sequences, combining them to a List (to use with Parser's `and` combiners) is undesireable;
	  * we can use this approach to avoid doing so.
	  *
	  * @return An intermediate object which has an `apply` and `flatMap` that can be used
	  *         to combine this Parser and a Transformer in a sequence.
	  */
	object followedByStream extends FollowedBy[({ type F[+T2] = Transformer[XMLEvent, T2] })#F, Out] {
		def apply[T2](getTransformer: Out => Transformer[XMLEvent, T2]) = new Transformer[XMLEvent, T2] {
			override def toString = s"$self.followedByStream($getTransformer)"
			def makeHandler[End](next: Handler[T2, End]): Handler[XMLEvent, End] = {
				val handler1 = self.makeHandler()
				def getHandler2(h1Result: Try[Out]): Handler[XMLEvent, End] = h1Result match {
					case Success(result) => getTransformer(result).makeHandler(next)
					case Failure(err) => TransformerHandler[XMLEvent, Nothing, End](next, _ => throw err)
				}
				new SequencedInStackHandler(handler1, getHandler2)
			}
		}
	}

	/** If a `Parser` is context-independent, it can be treated to a `Consumer`.
		*
		* @return A representation of this parser as a `Consumer`
		*/
	def toConsumer: Consumer[XMLEvent, Try[Out]] = {
		new Consumer[XMLEvent, Try[Out]] {
			def makeHandler(): Handler[XMLEvent, Try[Out]] = self.makeHandler()
			override def toString = self.toString
		}
	}

	def parse[XML](xml: XML)(
		implicit consumeXML: ConsumableLike[XML, XMLEvent]
	): Try[Out] = consumeXML(xml, makeHandler())
}

object Parser {
	def fromConsumer[Out](consumer: Consumer[XMLEvent, Try[Out]]): Parser[Out] = {
		new Parser[Out] {
			def makeHandler() = consumer.makeHandler()
			override def toString = consumer.toString
		}
	}

	// TEXT
	def forText: Parser[String] = ForText
	object ForText extends Parser[String] {
		def makeHandler() = new TextCollectorHandler
		override def toString = "XMLText"
	}

	// ATTRIBUTE
	def forMandatoryAttribute(name: QName): Parser[String] = new ForMandatoryAttribute(name)
	def forMandatoryAttribute(name: String): Parser[String] = new ForMandatoryAttribute(new QName(name))
	class ForMandatoryAttribute(name: QName) extends Parser[String] {
		def makeHandler() = new MandatoryAttributeHandler(name)
		override def toString = s"Attribute($name)"
	}

	// OPTIONAL ATTRIBUTE
	def forOptionalAttribute(name: QName): Parser[Option[String]] = new ForOptionalAttribute(name)
	def forOptionalAttribute(name: String): Parser[Option[String]] = new ForOptionalAttribute(new QName(name))
	class ForOptionalAttribute(name: QName) extends Parser[Option[String]] {
		def makeHandler() = new OptionalAttributeHandler(name)
		override def toString = s"OptionalAttribute($name)"
	}

	// CONSTANT
	def constant[A](result: A): Parser[A] = new Parser[A] {
		def makeHandler(): Handler[XMLEvent, Try[A]] = {
			new SafeConsumerHandler(new ConstantHandler(result))
		}
		override def toString = s"Constant($result)"
	}
}



















