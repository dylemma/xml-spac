package io.dylemma.spac

package object types {

	/** Wrapper for `T`, used when the compiler is looking for a type constructor e.g. `C[_]` */
	type Id[+T] = T

}
