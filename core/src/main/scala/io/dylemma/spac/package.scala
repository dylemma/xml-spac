package io.dylemma

import cats.data.Chain

package object spac {

	type Emit[+A] = Chain[A]
	val Emit: Chain.type = Chain

}
