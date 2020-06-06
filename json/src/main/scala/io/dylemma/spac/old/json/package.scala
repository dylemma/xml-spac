package io.dylemma.spac.old

import io.dylemma.spac.ContextMatcher
import io.dylemma.spac.old.json.syntax.{ContextMatcherSyntax, Implicits}

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
