package io.dylemma.spac

/** Marker trait used by `SpacTraceElement.InInput` to extract location information from inputs that cause parsing exceptions.
  *
  * @group errors
  */
trait HasLocation {
	def location: ContextLocation
}
