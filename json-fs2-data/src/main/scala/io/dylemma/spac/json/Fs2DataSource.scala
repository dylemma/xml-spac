package io.dylemma.spac
package json

import fs2._
import fs2.data.json.{Token, tokens}
import fs2.data.text.CharLikeChunks
import io.dylemma.spac.interop.fs2._
import io.dylemma.spac.json.impl.JsonStackFixer

/** Provides helpers for creating FS2 streams of `io.dylemma.spac.json.JsonEvent`,
  * using fs2-data-json as the underlying event provider.
  *
  * This helper is set up to mirror the layout of the corresponding Fs2DataSource helper in the fs2-data-xml support module,
  * but since there's less customization involved in setting up a stream of fs2-data-json Token objects,
  * there's a lot less going on in this object. The main functionality provided here is the `convert` pipe,
  * and some small helpers to automatically call it.
  *
  * For example:
  * {{{
  *    val charStream: Stream[IO, Char] = ???
  *
  *    // this...
  *    val jsonStream1: Stream[IO, JsonEvent] = {
  *      import fs2.data.json._
  *      charStream
  *        .through(tokens)
  *        .through(Fs2DataSource.convert)
  *    }
  *
  *    // could be replaced with
  *    val jsonStream2: Stream[IO, JsonEvent] = {
  *      Fs2DataSource[IO](charStream)
  *    }
  * }}}
  *
  * Essentially this helper just provides a convenient `apply` method that accepts
  * `String`, `Stream[F, Char]`, `Stream[F, String]`, or `Stream[F, fs2.data.json.Token]`
  * to return a `Stream[F, JsonEvent]` which a `JsonParser` could then interact with.
  *
  * @group support
  */
object Fs2DataSource {

	def fromRawJsonStream[F[_], A](rawJsonStream: Stream[F, A])(implicit A: CharLikeChunks[F, A], F: RaiseThrowable[F], callerPos: CallerPos): Stream[F, JsonEvent] = {
		rawJsonStream through tokens[F, A] through convert[F]
	}

	def fromString[F[_]](rawJson: String)(implicit F: RaiseThrowable[F], callerPos: CallerPos): Stream[F, JsonEvent] = {
		fromRawJsonStream(Stream.emit[F, String](rawJson))
	}

	/** Pipe for converting `fs2.data.json.Token` to `io.dylemma.spac.json.JsonEvent`.
	  * This will be used under the hood of the `apply` and `syncIO` helpers,
	  * but is being made available to allow for manual stream creation when desired.
	  * This pipe takes care of injecting "inferred" events that don't have explicit
	  * token representations, like FieldEnd, IndexStart, and IndexEnd.
	  */
	def convert[F[_]](implicit callerPos: CallerPos): Pipe[F, Token, JsonEvent] = Transformer
		.map(tokenToJsonEvent)
		.through(JsonStackFixer)
		.toPipe[F]

	private def tokenToJsonEvent(token: Token): JsonEvent = token match {
		case Token.StartObject => objectStart
		case Token.EndObject => objectEnd
		case Token.StartArray => arrayStart
		case Token.EndArray => arrayEnd
		case Token.Key(name) => JsonEvent.FieldStart(name, ContextLocation.empty)
		case Token.StringValue(s) => JsonEvent.JString(s, ContextLocation.empty)
		case Token.NumberValue(raw) => jsonNumber(raw)
		case Token.TrueValue => jsonTrue
		case Token.FalseValue => jsonFalse
		case Token.NullValue => jsonNull
	}

	private val objectStart = JsonEvent.ObjectStart(ContextLocation.empty)
	private val objectEnd = JsonEvent.ObjectEnd(ContextLocation.empty)
	private val arrayStart = JsonEvent.ArrayStart(ContextLocation.empty)
	private val arrayEnd = JsonEvent.ArrayEnd(ContextLocation.empty)
	private val jsonTrue = JsonEvent.JBool(true, ContextLocation.empty)
	private val jsonFalse = JsonEvent.JBool(false, ContextLocation.empty)
	private val jsonNull = JsonEvent.JNull(ContextLocation.empty)

	private def jsonNumber(raw: String) = {
		val asLong =
			try Some(raw.toLong)
			catch { case _: NumberFormatException => None }
		asLong.map(JsonEvent.JLong(_, ContextLocation.empty)).getOrElse {
			JsonEvent.JDouble(raw.toDouble, ContextLocation.empty)
		}
	}

}
