package io.dylemma.spac

object ContextualizedException {
	def apply[In](contextTrace: ContextTrace[In], cause: Throwable): Throwable = {
		if(contextTrace.elems.isEmpty) cause
		else new ContextualizedException(contextTrace, cause)
	}
}

class ContextualizedException[In] private(val contextTrace: ContextTrace[In], cause: Throwable) extends Exception({
	// private constructor guarded by companion object's `apply` method ensures `contextTrace` is non-empty
	contextTrace.elems.iterator
		.map { case (loc, in) => s"\t@ $loc - $in" }
		.mkString(s"Exception thrown from parsing logic: [\n", "\n", "\n]")
}, cause)