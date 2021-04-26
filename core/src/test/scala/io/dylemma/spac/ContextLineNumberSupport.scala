package io.dylemma.spac

case class ContextLineNumberSupport(canCheckLineNumbers: Boolean)
object ContextLineNumberSupport {
	val Disabled = ContextLineNumberSupport(false)
	val Enabled = ContextLineNumberSupport(true)
}
