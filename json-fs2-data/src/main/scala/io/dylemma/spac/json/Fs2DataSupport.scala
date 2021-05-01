package io.dylemma.spac
package json

import cats.Id
import cats.arrow.FunctionK
import cats.effect.{Sync, SyncIO}

/** Provides implicits that allow various data sources to be passed to a JsonParser's `parse` or `parseF` method,
  * using `fs2.data.json` as the the underlying JSON event generator.
  *
  * Provides an implicit `Parsable` instance for types belonging to the `Fs2DataSource.ToFs2JsonTokenStream` typeclass:
  *
  * - `String`
  * - `fs2.Stream[F, Char]`
  * - `fs2.Stream[F, String]`
  * - `fs2.Stream[F, Byte]` (note: requires additional imports from `fs2.data.text`)
  * - `fs2.Stream[F, fs2.data.json.Token]`
  *
  * Note that any `fs2.Stream[F, A]` is supported as long as there's an implicit `fs2.data.CharLikeChunks[F, A]` available.
  *
  * Note that `java.io.File` is not directly supported; you can use `fs2-io` to open the File as a stream of Char or Byte.
  *
  * @group support
  */
object Fs2DataSupport {

	/** Allows types which can be passed to `Fs2DataSource[F].apply` to also be passed to JsonParser's `parseF` method.
	  * The general idea is that an `S` can be converted to an `fs2.Stream[F, fs2.data.json.Token]`, and subsequently
	  * converted to a `fs2.Stream[F, io.dylemma.spac.json.JsonEvent]` which a `JsonParser` can understand.
	  *
	  * By default, `S` can be `String`, `Stream[F, String]`, `Stream[F, Char]`, or `Stream[F, fs2.data.json.Token]`.
	  */
	implicit def fs2DataJsonSourceAsParsableF[F[_], S](
		implicit F: Sync[F],
		S: Fs2DataSource.ToFs2JsonTokenStream[F, S],
		callerPos: CallerPos
	): Parsable[F, S, JsonEvent] = {
		Parsable.forFs2Stream[F, JsonEvent].contramapSource(Fs2DataSource[F](_))
	}

	/** Allows fs2-data-json streams in the `SyncIO` effect to be consumed by JsonParsers in a blocking manner via the `parse` method.
	  *
	  * @param S Evidence that `S` either *is* or can be converted to, a stream of `fs2.data.json.Token` in the SyncIO context
	  * @tparam S A type that either *is*, or can be converted to, a stream of fs2.data.json.Token in the SyncIO context
	  */
	implicit def fs2DataJsonSourceAsIdParsable[S](
		implicit S: Fs2DataSource.ToFs2JsonTokenStream[SyncIO, S]
	): Parsable[cats.Id, S, JsonEvent] = {
		fs2DataJsonSourceAsParsableF[SyncIO, S].mapK(new FunctionK[SyncIO, cats.Id] {
			def apply[A](fa: SyncIO[A]): Id[A] = fa.unsafeRunSync()
		})
	}
}
