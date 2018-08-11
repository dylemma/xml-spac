package io.dylemma.spac

import io.dylemma.spac.json.syntax.{ContextMatcherSyntax, Implicits}

package object json
	extends Implicits
	with ContextMatcherSyntax
{
	object Implicits extends Implicits
	object ContextMatcherSyntax extends ContextMatcherSyntax
}
