package xsp

import java.util.concurrent.atomic.AtomicBoolean

/**
	* Created by dylan on 5/14/2016.
	*/
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
