package io.dylemma

import cats.data.Chain

package object spac {

	/** Type alias used by the Transformer.Handler methods, to represent a series of outputs to "emit". It's just `cats.data.Chain`. */
	type Emit[+A] = Chain[A]
	/** Value alias that acts as the `Emit[A]` companion, used by Transformer.Handler implementations. It's just `cats.data.Chain`. */
	val Emit: Chain.type = Chain

}
