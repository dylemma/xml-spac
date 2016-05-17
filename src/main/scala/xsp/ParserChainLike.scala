package xsp

import javax.xml.stream.events.XMLEvent

import xsp.handlers.CompoundHandler

import scala.annotation.implicitNotFound
import scala.language.implicitConversions
import scala.{Tuple2 => ~}

/** Type that witnesses the fact that a "chain" of `Parser`s
	* (e.g. `(((parser1, parser2), parser3), parser4)...` )
	* can be combined into a single `Parser` whose context is type `C`
	* and whose results are tuples of type `T`.
	*
	* @tparam P A type representing a chain of parsers. A chain is actually
	*           a series of nested tuples, e.g. `(((A, B), C), D)`. Such
	*           tuples can easily be created with scala's builtin arrow
	*           operator e.g. `A -> B -> C -> D`.
	* @tparam C The most-specific context type among the parsers in `P`.
	*           For example in a chain of parsers with context types
	*           `Seq[X]`, `List[X]`, and `Any`, the most specific context
	*           type (i.e. the value of type parameter `C`) will be `List[X]`.
	*           Note that the context types *MUST* all share a common subtype.
	*           A chain of parsers with context types `Int`, `String`, and `Any`
	*           would not work because there is no value that can be passed as
	*           a context of both `Int` and `String`. Attempting to do so will
	*           result in a compilation error.
	* @tparam T A tuple type which represents the combined results of the
	*           parsers in the chain `P`. For example, in a chain of parsers
	*           whose results are of type `A`, `B`, `C`, and `D` respectively,
	*           the result type `T` will be the tuple type `(A, B, C, D)`.
	*/
@implicitNotFound(
	"Could not prove that ${P} is a chain of parsers with a common context type. " +
	"The most likely cause is that the context types of the parsers do not lie in the same type hierarchy. " +
	"It is also possible that a member of the chain is not actually a parser."
)
sealed trait ParserChainLike[P, C, T] {
	def makeHandler(chain: P, context: C): Handler[XMLEvent, Result[T]]
}

object ParserChainLike {
	implicit def getInstance[
		P <: (_, Parser[_, _]), // original parser chain type
		C, // normalized context type for parsers in P
		Pn <: (_, Parser[C, _]), // context-normalized parser chain type
		R, // intermediate type to represent the result-chain for the parsers
		T // tuple type representing the results of all parsers in P
	](
		implicit contextNormalizer: ParserChainNormalizer[P, Pn, C],
		handlerSetup: ParserChainSetup[C, Pn, R],
		resultTupler: ResultChainTuple[R, T]
	): ParserChainLike[P, C, T] = new ParserChainLike[P, C, T] {
		def makeHandler(chain: P, context: C): Handler[XMLEvent, Result[T]] = {
			val normalizedChain = contextNormalizer.normalize(chain)
			val innerHandlers = handlerSetup.createHandlers(normalizedChain, context)
			val formResult = handlerSetup.formResultChain _ andThen resultTupler.toTuple _
			new CompoundHandler(innerHandlers, formResult)
		}
	}

	// ========================================================================
	//                       PARSER CHAIN NORMALIZATION
	// ========================================================================

	/** Typeclass that knows how to convert an arbitrary parser chain `P` to a
		* "normalized" parser chain `Pn` where each of the parsers in the normalized
		* chain take a context of type `C`.
		*
		* Valid instances of `ParserChainNormalizer` should no-op; a parser chain
		* that can be normalized is one that has a common context type, which the
		* normalizer will find and use. Since parsers are contravariant on Context,
		* the normalization process will simply be treated as a supertype of itself.
		*
		* @tparam P The raw parser chain type
		* @tparam Pn The normalized parser chain type
		* @tparam C The context type for parsers in the normalized chain
		*/
	sealed trait ParserChainNormalizer[P, Pn, C] {
		def normalize(parsers: P): Pn
	}
	object ParserChainNormalizer {
		implicit def getInstance[P <: (_, Parser[_, _]), C, Pn <: (_, Parser[_, _])](
			implicit baseType: CommonContextFinder[P, C],
			caster: ContextCaster[P, C, Pn]
		): ParserChainNormalizer[P, Pn, C] = new ParserChainNormalizer[P, Pn, C] {
			def normalize(parsers: P): Pn = caster.castContext(parsers)
		}
	}

	/** Typeclass that knows how to find the common context type `C` which is a subtype
		* of the context types of each parser in the parser chain type `P`.
		* This trait exists only to encode type-level information, and has no actual functionality.
		*
		* @tparam P The parser chain type
		* @tparam C The common context subtype for parsers in the chain type `P`
		*/
	@implicitNotFound("could not find a common context among the parsers in ${P}")
	sealed trait CommonContextFinder[P, C]
	object CommonContextFinder {
		implicit def singleParserBaseType[C, T]: CommonContextFinder[Parser[C, T], C] = null
		implicit def pairedParserBaseType[P, Pc, Tc, Tt, C](
			implicit prefixTyper: CommonContextFinder[P, Pc],
			tailTyper: CommonContextFinder[Parser[Tc, Tt], Tc],
			commonContext: MostSpecificType[C, Pc, Tc]
		): CommonContextFinder[P ~ Parser[Tc, Tt], C] = null
	}

	/** Typeclass that knows how to safely convert a parser chain of type `P`, to a new
		* chain type `Pc` whose parsers all have a context of type `C`.
		*
		* Valid instances of `ContextCaster` will all essentially no-op; Parsers are
		* contravariant on their context types, and instances of this typeclass will only
		* be created for parsers with a context type such that `C <: parser#Context`.
		*
		* @tparam P The input parser chain type
		* @tparam C The new parser context type
		* @tparam Pc The "cast" version of the parser chain type
		*/
	trait ContextCaster[P, C, Pc] {
		def castContext(parsers: P): Pc
	}
	object ContextCaster {
		implicit def one[Pc, Pt, C <: Pc]: ContextCaster[Parser[Pc, Pt], C, Parser[C, Pt]] = {
			new ContextCaster[Parser[Pc, Pt], C, Parser[C, Pt]] {
				def castContext(parser: Parser[Pc, Pt]): Parser[C, Pt] = parser
			}
		}
		implicit def more[P, Pc, Tc, Tt, C <: Tc](implicit prefixCaster: ContextCaster[P, C, Pc])
		: ContextCaster[P ~ Parser[Tc, Tt], C, Pc ~ Parser[C, Tt]] = {
			new ContextCaster[P ~ Parser[Tc, Tt], C, Pc ~ Parser[C, Tt]] {
				def castContext(parsers: P ~ Parser[Tc, Tt]): (Pc, Parser[C, Tt]) = {
					prefixCaster.castContext(parsers._1) -> parsers._2
				}
			}
		}
	}

	// ========================================================================
	//                      PARSER CHAIN HANDLER SETUP
	// ========================================================================

	/** Typeclass that knows how to initialize individual handlers for all of the
		* parsers in the parser-chain type `P`, and combine their results back to
		* a result chain type `R`.
		*
		* @tparam Context A common context type for all parsers in the chain `P`
		* @tparam P A parser chain type e.g. `Parser[C1, T1] -> Parser[C2, T2] -> ...`
		* @tparam R A result chain type e.g. `Result[T1] -> Result[T2] -> ...`
		*/
	sealed trait ParserChainSetup[Context, P, R] {
		/** The length of the parser chain. */
		def length: Int

		/** Create handlers appropriate to the `parsers` chain, and insert them
			* into the `handlers` array at positions corresponding to each parser's
			* position in the `parsers` chain.
			*
			* @param parsers The parser chain
			* @param context A context value to pass to each parser in the parser chain
			* @param handlers An array to fill with newly-created handlers
			*/
		protected def setupHandlers(
			parsers: P,
			context: Context,
			handlers: Array[Handler[XMLEvent, Result[Any]]]
		)

		/** Given a sequence of results, form a chain of `Result` values
			* from the individual results in the seq. This will generally
			* involve unchecked casts, so we hide this logic behind a bunch
			* of type safety so that by the time we get down here, we can
			* assume that the casts will *always* succeed. Don't try this at
			* home, kids!
			*
			* @param rawResults The results of handlers created by `createHandlers`
			* @return A `Result` chain based on the values in `rawResults`
			*/
		def formResultChain(rawResults: IndexedSeq[Result[Any]]): R

		/** Create a sequence of `Handler`s corresponding to the given parser chain.
			*
			* @param parsers The parser chain
			* @param context A context value to pass to each parser in the parser chain
			* @return A sequence of fresh `Handler` instances which correspond to the
			*         parsers in the given `parsers` chain.
			*/
		def createHandlers(
			parsers: P,
			context: Context
		): IndexedSeq[Handler[XMLEvent, Result[Any]]] = {
			val handlers = new Array[Handler[XMLEvent, Result[Any]]](length)
			setupHandlers(parsers, context, handlers)
			handlers
		}
	}

	object ParserChainSetup {

		/** Implicitly get an instance of `ParserChainEndSetup` */
		implicit def endCase[Context, A]: ParserChainSetup[Context, Parser[Context, A], Result[A]] = {
			new ParserChainEndSetup[Context, A]
		}

		/** Implicitly get an instance of `ParserChainLinkSetup` */
		implicit def linkCase[Context, PrefixP, PrefixR, A]
		(implicit prefixCase: ParserChainSetup[Context, PrefixP, PrefixR])
		: ParserChainSetup[Context, PrefixP ~ Parser[Context, A], PrefixR ~ Result[A]] = {
			new ParserChainLinkSetup[Context, PrefixP, PrefixR, A]
		}

		/** Inductive case for `ParserChainSetup`.
			* Given a chain that consists of some "Prefix" chain and some "Tail" parser,
			* and given the setup knowledge for the Prefix chain, we can set up the
			* combination chain of Prefix -> Tail
			*
			* @param headSetup A `ParserChainSetup` that can set up the prefix of the parser chain
			* @tparam Context A common context type for all parsers in the chain `P`
			* @tparam PrefixP The parser chain's prefix type
			* @tparam PrefixR The type of the results from the parser chain's prefix
			* @tparam A The result type of the parser at the end of the chain
			*/
		class ParserChainLinkSetup[Context, PrefixP, PrefixR, A]
		(implicit headSetup: ParserChainSetup[Context, PrefixP, PrefixR])
			extends ParserChainSetup[Context, PrefixP ~ Parser[Context, A], PrefixR ~ Result[A]] {

			val length = headSetup.length + 1
			protected def setupHandlers(
				parsers: PrefixP ~ Parser[Context, A],
				context: Context,
				handlers: Array[Handler[XMLEvent, Result[Any]]]
			): Unit = {
				val tailParser = parsers._2
				val headChain = parsers._1
				val handler = tailParser.makeHandler(context)
				handlers(length - 1) = handler
				headSetup.setupHandlers(headChain, context, handlers)
			}
			def formResultChain(rawResults: IndexedSeq[Result[Any]]): PrefixR ~ Result[A] = {
				headSetup.formResultChain(rawResults) -> rawResults(length - 1).asInstanceOf[Result[A]]
			}
		}

		/** Inductive base case for `ParserChainSetup`, which knows how to
			* set up a chain of size 1 (i.e. a single Parser).
			*
			* @tparam Context The single parser's context type
			* @tparam A The single parser's result type
			*/
		class ParserChainEndSetup[Context, A]
			extends ParserChainSetup[Context, Parser[Context, A], Result[A]] {

			val length = 1
			protected def setupHandlers(
				parser: Parser[Context, A],
				context: Context,
				handlers: Array[Handler[XMLEvent, Result[Any]]]
			): Unit = {
				handlers(0) = parser.makeHandler(context)
			}
			def formResultChain(rawResults: IndexedSeq[Result[Any]]): Result[A] = {
				rawResults(0).asInstanceOf[Result[A]]
			}
		}

	}
}

