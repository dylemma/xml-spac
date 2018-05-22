package io.dylemma.spac.json.syntax

import io.dylemma.spac.json.JsonParser

trait Aliases {
	type Parser[+A] = JsonParser[A]
	val Parser = JsonParser
}
