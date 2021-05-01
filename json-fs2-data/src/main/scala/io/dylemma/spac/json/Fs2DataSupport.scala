package io.dylemma.spac
package json

import cats.Id
import cats.arrow.FunctionK
import cats.effect.{Sync, SyncIO}

/** @group support */
object Fs2DataSupport {
	implicit def fs2DataJsonSourceAsParsableF[F[_], S](
		implicit F: Sync[F],
		S: Fs2DataSource.ToFs2JsonTokenStream[F, S],
		callerPos: CallerPos
	): Parsable[F, S, JsonEvent] = {
		Parsable.forFs2Stream[F, JsonEvent].contramapSource(Fs2DataSource[F](_))
	}

	implicit def fs2DataJsonSourceAsIdParsable[S](
		implicit S: Fs2DataSource.ToFs2JsonTokenStream[SyncIO, S]
	): Parsable[cats.Id, S, JsonEvent] = {
		fs2DataJsonSourceAsParsableF[SyncIO, S].mapK(new FunctionK[SyncIO, cats.Id] {
			def apply[A](fa: SyncIO[A]): Id[A] = fa.unsafeRunSync()
		})
	}
}
