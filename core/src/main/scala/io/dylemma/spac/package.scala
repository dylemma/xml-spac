package io.dylemma

import cats.MonadError
import cats.data.Chain

package object spac {
	type Emit[+A] = Chain[A]
	val Emit: Chain.type = Chain

	type MonadThrow[F[_]] = MonadError[F, Throwable]
}
