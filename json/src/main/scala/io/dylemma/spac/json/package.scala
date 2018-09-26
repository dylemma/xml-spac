package io.dylemma.spac

import io.dylemma.spac.json.JsonStackElem
import io.dylemma.spac.json.syntax.{ContextMatcherSyntax, Implicits}

package object json
	extends Implicits
	with ContextMatcherSyntax
{
	object Implicits extends Implicits
	object ContextMatcherSyntax extends ContextMatcherSyntax

	type JsonParser[+A] = Parser[JsonEvent, A]
	type JsonTransformer[+A] = Transformer[JsonEvent, A]
	type JsonContextMatcher[+A] = ContextMatcher[JsonStackElem, A]
}
