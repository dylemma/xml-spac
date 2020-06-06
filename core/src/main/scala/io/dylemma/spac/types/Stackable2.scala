package io.dylemma.spac.types

import io.dylemma.spac.{ContextChange, Transformer}

trait Stackable2[F[+_], In, +Elem] {
	def interpret: Transformer[F, In, Either[ContextChange[In, Elem], In]]
}
