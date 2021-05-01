package io.dylemma.spac

import io.dylemma.spac.impl.{SplitterByConsecutiveMatches, SplitterByContextMatch, SplitterByInputMatch, SplitterJoiner}

/** Primary "spac" abstraction that acts as a selector for sub-streams within a single input stream.
  *
  * A "sub-stream" is some series of consecutive values from the original stream, identified by a "context" value.
  * Sub-streams do not overlap with each other.
  *
  * For example, when handling a stream of XML events, you might want to create a Splitter that identifies
  * the events representing elements at a specific location within the XML; something like an XPATH that operates on streams.
  * When using `xml-spac`, you might construct a splitter like `Splitter.xml("rootElem" \ "things" \ "thing")`.
  * This would identify a new sub-stream for each `<thing>` element that appears inside a `<things>` element, inside the `<rootElem>` element.
  * An example sub-stream for a `<thing>` element might be `ElemStart("thing"), Text("hello"), ElemEnd("thing")`.
  *
  * A Splitter's general goal is to attach a Parser or Transformer to each sub-stream, passing the contents of that sub-stream
  * through the attached Parser or Transformer in order to get an interpretation of that sub-stream (i.e. the Parser's result,
  * or some emitted outputs from a Transformer).
  * With the `<thing>` example above, you might attach a parser that concatenates the context all Text events it sees.
  * I.e. `XmlParser.forText`. Since a separate parser handler will run for each sub-stream, this becomes something like
  * "A stream of Strings which each represent the concatenated text from an individual `<thing>` element".
  *
  * @tparam In Data event type for the input stream
  * @tparam C  Context type used to identify each sub-stream
  * @group primary
  */
trait Splitter[In, +C] {

	/** Inject "boundary" events into an input stream, where a `ContextPush` represents the
	  * beginning of a new sub-stream, and a `ContextPop` represents the end of a sub-stream.
	  *
	  * @return A transformer that injects the boundary events into any given input stream
	  */
	def addBoundaries: Transformer[In, Either[ContextChange[In, C], In]]

	/** Creates a new transformer by attaching a new parser to each sub-stream based on the sub-stream context.
	  * For each sub-stream, a new parser will be created, and inputs from the sub-stream will be piped into that parser.
	  * When the sub-stream ends, or if the parser finishes on its own, the parser's result will be emitted as an `Out` event.
	  *
	  * @param parseMatches Given the context for a sub-stream, return a parser to handle that sub-stream
	  * @tparam Out The parser's output type
	  * @return A transformer that will emit the result of each parsed sub-stream
	  */
	def map[Out](parseMatches: C => Parser[In, Out]): Transformer[In, Out] = mapTraced(push => parseMatches(push.context))

	/** Like `map`, but using the `ContextPush` associated with the sub-stream, instead of just the context value itself.
	  *
	  * @param parseMatches
	  * @tparam Out
	  * @return
	  */
	def mapTraced[Out](parseMatches: ContextPush[In, C] => Parser[In, Out]): Transformer[In, Out] = flatMap { push =>
		parseMatches(push).asTransformer
	}

	/** Like `map`, but when you want to use the same parser for each sub-stream, regardless of the context value
	  */
	def joinBy[Out](parser: Parser[In, Out]): Transformer[In, Out] = mapTraced(new ConstFunction(parser))

	/** Like `joinBy`, but the parser is passed implicitly
	  */
	def as[Out](implicit parser: Parser[In, Out]) = mapTraced(new ConstFunction(parser))

	private class ConstFunction[A](result: A) extends (Any => A) {
		def apply(v1: Any) = result
		override def toString = result.toString
	}

	/** Creates a new transformer by attaching an "inner" transformer to each sub-stream based on the sub-stream context.
	  * For each sub-stream, a new transformer will be created, and the inputs from the sub-stream will be piped into the inner transformer.
	  * Anything that the inner transformer emits will be emitted by the returned transformer.
	  */
	def flatMap[Out](transformMatches: ContextPush[In, C] => Transformer[In, Out]): Transformer[In, Out] = addBoundaries through SplitterJoiner(transformMatches)
}

/**
  * @group primary
  */
object Splitter {
	/** Convenience for creating Splitters with a specific `In` type; useful when type inference can figure out the other type parameters. */
	def apply[In] = new SplitterApplyWithBoundInput[In]

	/** Create a splitter that keeps track of a "stack" which is pushed and popped by `In` events,
	  * starting a new substream when the given `matcher` matches the stack.
	  *
	  * The primary use-case for this is when dealing with nestable data formats like XML or JSON,
	  * where a token could signify a push to the stack (e.g. an ElemStart event), and where you
	  * want to operate on events that occur within some specific stack of elements.
	  *
	  * For inputs that cause a push or pop to the stack, whether that input is included as "inside"
	  * the pushed context is up to the specific `StackLike` implementation.
	  */
	def fromMatcher[In, Elem, C](matcher: ContextMatcher[Elem, C])(implicit S: StackLike[In, Elem], pos: CallerPos): Splitter[In, C] = new SplitterByContextMatch(matcher, pos)

	/** Create a splitter that starts a new substream every time the `matcher` matches.
	  * Any events passed through before the initial match will be discarded, but every event
	  * thereafter will be part of a substream. The context for a substream is based on the
	  * value returned by the `matcher` for the event that caused that match.
	  *
	  * For example, in a stream like `4 3 2 1 2 3 1 2 1 2 3 4`, if our matcher was `{ case 1 => "yay" }`,
	  * then we'd have a new substream with context "yay" every time a `1` came through:
	  *
	  *  - (new context: "yay") 1 2 3
	  *  - (new context: "yay") 1 2
	  *  - (new context: "yay") 1 2 3 4
	  *
	  * @param matcher A PartialFunction that can extract a context value from inputs
	  * @tparam In The input type
	  * @tparam C  The extracted context type
	  * @return A splitter that starts a new substream for every input where `matcher.isDefinedAt(input)`,
	  *         with a context equal to `matcher(input)`.
	  */
	def splitOnMatch[In, C](matcher: PartialFunction[In, C]): Splitter[In, C] = new SplitterByInputMatch(matcher)

	/** Create a splitter that starts a new substream every time the predicate function `p` returns true for an input.
	  * Any inputs passed through before the initial match will be discarded, but every event thereafter will be part
	  * of a substream. Context is ignored for substreams from this method - the context type is `Any`.
	  *
	  * For example, in a stream like `4 3 2 1 2 3 1 2 1 2 3 4`, if our predicate was `{ _ == 1 }`,
	  * then we'd have a new substream starting from each `1` input.
	  *
	  *  - (new context) 1 2 3
	  *  - (new context) 1 2
	  *  - (new context) 1 2 3 4
	  *
	  * @param f The predicate function responsible for determining if a new context should start for an input.
	  * @tparam In The input type
	  * @return A splitter that starts a new substream for every input where `p(input) == true`
	  */
	def splitOnMatch[In](f: In => Boolean): Splitter[In, Any] = splitOnMatch[In, Unit] { case in if f(in) => () }

	/** Create a Splitter that treats consecutive matched values as substreams.
	  * For example, given a matcher like `{ case c if c.isLetter => c }`, a stream like
	  * {{{1 2 3 A B C 4 5 6 D 7 8 E F G H 9}}}
	  * could be treated as having three substreams, where each substream's "context value"
	  * is the first letter in that group (because context is always defined by the beginning
	  * of the substream).
	  *
	  *  - `A B C` with context `'A'` (between the 3 and 4)
	  *  - `D` with context `'D'` (between the 6 and 7)
	  *  - `E F G H` with context `'E'` (between the 8 and 9)
	  *
	  * @param matcher A function defining which inputs count as a "match"
	  * @tparam In
	  * @tparam Context
	  * @return
	  */
	def consecutiveMatches[In, Context](matcher: PartialFunction[In, Context]): Splitter[In, Context] = new SplitterByConsecutiveMatches(matcher)

	/** Create a Splitter that treats consecutive values matching the predicate `p` as
	  * substreams with no particular context value.
	  * For example, given a matcher like `i => i % 2 == 0`, a stream like
	  * {{{1 3 2 2 4 5 6 7 8 10 4 3 1}}}
	  * could be treated as having three substreams:
	  *
	  *  - `2 2 4`
	  *  - `6`
	  *  - `8 10 4`
	  *
	  * @param p
	  * @tparam In
	  * @return
	  */
	def consecutiveMatches[In](p: In => Boolean): Splitter[In, Any] = consecutiveMatches[In, Any] { case in if p(in) => () }
}

/**
  * @tparam In
  * @group util
  */
class SplitterApplyWithBoundInput[In] {
	/** See `Splitter.fromMatcher` */
	def fromMatcher[Elem, C](matcher: ContextMatcher[Elem, C])(implicit S: StackLike[In, Elem], pos: CallerPos): Splitter[In, C] = Splitter.fromMatcher(matcher)
	/** See `Splitter.splitOnMatch` */
	def splitOnMatch[C](matcher: PartialFunction[In, C]): Splitter[In, C] = Splitter.splitOnMatch(matcher)
	/** See `Splitter.splitOnMatch` */
	def splitOnMatch(f: In => Boolean): Splitter[In, Any] = Splitter.splitOnMatch(f)
	/** See `Splitter.consecutiveMatches` */
	def consecutiveMatches[Context](matcher: PartialFunction[In, Context]): Splitter[In, Context] = Splitter.consecutiveMatches(matcher)
	/** See `Splitter.consecutiveMatches` */
	def consecutiveMatches(p: In => Boolean): Splitter[In, Any] = Splitter.consecutiveMatches(p)
}
