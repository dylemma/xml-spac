package io.dylemma.xsp

import java.util.concurrent.atomic.AtomicBoolean

private[xsp] object debug {
	val enabled = new AtomicBoolean(
		System.getProperty("xsp.debug.enabled") == "true"
	)

	def apply(msg: String) = {
		if(enabled.get) println(msg)
	}

	def as[T](msg: String) = { value: T =>
		if(enabled.get) println(s"$msg: $value")
		value
	}
}
