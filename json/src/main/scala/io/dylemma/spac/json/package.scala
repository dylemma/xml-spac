package io.dylemma.spac

import io.dylemma.spac.json.syntax.{Aliases, ContextMatcherSyntax, Implicits}

package object json
	extends Implicits
	with ContextMatcherSyntax
	with Aliases
{
	object Implicits extends Implicits
	object ContextMatcherSyntax extends ContextMatcherSyntax
	object Aliases extends Aliases
}
