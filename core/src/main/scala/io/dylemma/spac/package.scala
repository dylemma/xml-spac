package io.dylemma

import cats.data.Chain

package object spac
	extends ToPullable.Ops
{

	type Emit[+A] = Chain[A]
	val Emit: Chain.type = Chain

}
