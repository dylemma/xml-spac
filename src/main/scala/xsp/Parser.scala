package xsp

import javax.xml.namespace.QName
import javax.xml.stream.events.XMLEvent

import xsp.handlers._

import scala.util.control.NonFatal

trait Parser[-Context, +Out] { self =>
	def makeHandler(context: Context): Handler[XMLEvent, Result[Out]]

	def mapResult[B](f: Result[Out] => Result[B]): Parser[Context, B] = new Parser[Context, B] {
		def makeHandler(context: Context) = new MappedHandler(f, self.makeHandler(context))
	}

	def map[B](f: Out => B): Parser[Context, B] = mapResult(_ map f)

	/** Bind this `Parser` to a specific `context`.
		* The resulting parser ignores all context information passed to it for
		* purposes of creating a handler; it instead uses the context passed to
		* this method.
		*
		* @param context The `Context` value that will be used to create handlers
		*/
	def inContext(context: Context): Parser[Any, Out] = new Parser[Any, Out] {
		def makeHandler(ignored: Any) = self.makeHandler(context)
	}

	/** If a `Parser` is context-independent, it can be treated to a `Consumer`.
		*
		* @param ev Implicit evidence that the parser's `Context` type is `Any`
		* @return A representation of this parser as a `Consumer`
		*/
	def toConsumer(implicit ev: Any <:< Context): Consumer[XMLEvent, Result[Out]] = {
		new Consumer[XMLEvent, Result[Out]] {
			def makeHandler(): Handler[XMLEvent, Result[Out]] = self.makeHandler(ev(()))
		}
	}

	def parse[XML](xml: XML)(
		implicit consumeXML: ConsumableLike[XML, XMLEvent],
		anyContext: Any <:< Context
	): Result[Out] = consumeXML(xml, makeHandler(anyContext(())))
}
object Parser {
	def fromConsumer[Out](consumer: Consumer[XMLEvent, Result[Out]]): Parser[Any, Out] = {
		new Parser[Any, Out] {
			def makeHandler(context: Any) = consumer.makeHandler()
		}
	}

	// TEXT
	def forText: Parser[Any, String] = ForText
	object ForText extends Parser[Any, String] {
		def makeHandler(context: Any) = new TextCollectorHandler
	}

	// CONTEXT
	def forContext[C]: Parser[C, C] = new ForContext[C]
	class ForContext[C] extends Parser[C, C] {
		def makeHandler(context: C) = new OneShotHandler(Result.Success(context))
	}

	// ATTRIBUTE
	def forMandatoryAttribute(name: QName): Parser[Any, String] = new ForMandatoryAttribute(name)
	def forMandatoryAttribute(name: String): Parser[Any, String] = new ForMandatoryAttribute(new QName(name))
	class ForMandatoryAttribute(name: QName) extends Parser[Any, String] {
		def makeHandler(context: Any) = new MandatoryAttributeHandler(name)
	}

	// OPTIONAL ATTRIBUTE
	def forOptionalAttribute(name: QName): Parser[Any, Option[String]] = new ForOptionalAttribute(name)
	def forOptionalAttribute(name: String): Parser[Any, Option[String]] = new ForOptionalAttribute(new QName(name))
	class ForOptionalAttribute(name: QName) extends Parser[Any, Option[String]] {
		def makeHandler(context: Any) = new OptionalAttributeHandler(name)
	}

	// CHOOSE
	def choose[Context] = new ChooseApply[Context]
	class ChooseApply[Context] {
		def apply[Out](chooser: Context => Parser[Context, Out]): Parser[Context, Out] = {
			new Parser[Context, Out] {
				def makeHandler(context: Context): Handler[XMLEvent, Result[Out]] = {
					try chooser(context).makeHandler(context)
					catch {
						case NonFatal(err) =>
							val wrapped = new Exception(s"Failed to choose a parser for context [$context]", err)
							new OneShotHandler(Result.Error(wrapped))
					}
				}
			}
		}
	}

	/** Create a single parser that combines the results of the `parsers` as a tuple.
		*
		* Example:
		* {{{
		*   val parser1: Parser[Any, String] = ???
		*   val parser2: Parser[Int, Foo] = ??? // this Parser requires an Int for context
		*   val parser3: Parser[Any, Bar] = ???
		*
		*   val combinedParser: Parser[Int, (String, Foo, Bar)] = Parser.compound(
		*     parser1 ->
		*     parser2 ->
		*     parser3
		*   )
		*
		* }}}
		*
		* Note that the example above used scala's pair helper (`->`) to create a parser chain.
		* The following expression is identical to the above:
		* {{{
		*   val parserChain = ((parser1, parser2), parser3)
		*   val combinedParser = Parser.compound(parserChain)
		* }}}
		*
		* Note that when creating a compound parser, there must be a common "Context" type
		* which is a subtype of *all* of the context types of the parsers in the chain. For
		* example, you could not create a compound parser out of a `Parser[Int, Foo]` and a
		* `Parser[String, Bar]` because there is no type that is both an `Int` and a `String`.
		*
		* As a counterexample, the combination of a `Parser[Option[Int], Foo]` and a
		* `Parser[Some[Int], Bar]` would take a `Some[Int]` as context, since that is a subtype
		* of both `Some[Int]` and `Option[Int]`.
		*
		* @param parsers A chain of parsers, e.g. `((parser1, parser2), parser3)` or
		*                `parser1 -> parser2 -> parser3`. The chain can be of arbitrary
		*                length up to 22 as long as the overall chain is represented as
		*                a `Tuple2[ChainPrefix, LastParser]`.
		* @param p An implicit type evidence object that determines the common Context
		*          type and resulting tuple type.
		* @tparam Context A type that is a subtype of each of the `parsers`. This will
		*                 be used as the context type of the resulting parser.
		*                 For example in a chain of parsers with context types
		*                 `Seq[X]`, `List[X]`, and `Any`, the most specific context
		*                 type (i.e. the value of type parameter `C`) will be `List[X]`.
		*                 Note that the context types *MUST* all share a common subtype.
		*                 A chain of parsers with context types `Int`, `String`, and `Any`
		*                 would not work because there is no value that can be passed as
		*                 a context of both `Int` and `String`. Attempting to do so will
		*                 result in a compilation error.
		* @tparam P A type representing a chain of parsers. A chain is actually
		*           a series of nested tuples, e.g. `(((A, B), C), D)`. Such
		*           tuples can easily be created with scala's builtin arrow
		*           operator e.g. `A -> B -> C -> D`.
		* @tparam T A tuple type which represents the combined results of the
		*           parsers in the chain type `P`. For example, in a chain of parsers
		*           whose results are of type `A`, `B`, `C`, and `D` respectively,
		*           the result type `T` will be the tuple type `(A, B, C, D)`.
		* @return A single parser that will create a handler for each of the given `parsers`,
		*         passing events through each of them, eventually combining their results as
		*         as a tuple of type `T`.
		*/
	def compound[Context, P <: (_, Parser[_, _]), T](parsers: P)
		(implicit p: ParserChainLike[P, Context, T])
		: Parser[Context, T] = new Parser[Context, T] {
		def makeHandler(context: Context) = p.makeHandler(parsers, context)
	}
}



















