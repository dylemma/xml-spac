package io.dylemma.spac

import scala.reflect.ClassTag

class MissingFirstException[Out: ClassTag] extends NoSuchElementException(
	s"No ${implicitly[ClassTag[Out]]} was encountered before the end of its input."
)