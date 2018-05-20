package io.dylemma.spac.xml.syntax

import io.dylemma.spac.xml.XMLParser

/** Defines XML-specific aliases for classes from the core `spac` package.
  */
trait Aliases {
	type Parser[+A] = XMLParser[A]
	val Parser = XMLParser
}
