package io.dylemma.spac
package json

import cats.effect.SyncIO
import fs2._
import fs2.data.json.{Token, tokens}
import fs2.data.text.CharLikeChunks
import io.dylemma.spac.json.impl.JsonStackFixer

/** @group support */
object Fs2DataSource {

	/** Partial apply method for creating a `Stream[F, JsonEvent]` from some `source` */
	def apply[F[_]] = new Fs2DataSourcePartiallyApplied[F]
	/** Partial apply method for creating a `Stream[SyncIO, JsonEvent]` from some `source` */
	def syncIO = new Fs2DataSourcePartiallyApplied[SyncIO]

	class Fs2DataSourcePartiallyApplied[F[_]] {
		/** Given an implicit conversion from the `source` to a stream of fs2-data-json "Token" values,
		  * apply that conversion and then pipe the resulting stream through the `convert` pipe to get
		  * a stream of `JsonEvents` which can be parsed by json-spac parsers.
		  *
		  * Via the default-available implicits, the source can be a `String`, a `Stream[F, String]`,
		  * a `Stream[F, Char]`, or a `Stream[F, fs2.data.json.Token]`.
		  */
		def apply[S](source: S)(implicit S: ToFs2JsonTokenStream[F, S], callerPos: CallerPos): Stream[F, JsonEvent] = {
			S(source) through convert[F]
		}
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

	trait ToFs2JsonTokenStream[F[_], -S] {
		def apply(source: S): Stream[F, Token]
	}
	object ToFs2JsonTokenStream {
		/** No-op conversion for an already-existing stream of fs2-data-json Tokens */
		implicit def identity[F[_]]: ToFs2JsonTokenStream[F, Stream[F, Token]] = source => source

		/** Pipes a stream of string/char through the fs2-data-json `tokens` pipe */
		implicit def fromRawJsonChunks[F[_], T](implicit F: RaiseThrowable[F], T: CharLikeChunks[F, T]): ToFs2JsonTokenStream[F, Stream[F, T]] = {
			_ through tokens[F, T]
		}

		/** Pipes a single string (assumed to contain a complete JSON value) through the fs2-data-json `tokens` pipe */
		implicit def fromRawJson[F[_]](implicit F: RaiseThrowable[F]): ToFs2JsonTokenStream[F, String] = {
			source => Stream.emit(source) through tokens[F, String]
		}
	}
}
