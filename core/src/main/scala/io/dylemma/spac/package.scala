package io.dylemma

import io.dylemma.spac.syntax._

package object spac
	extends TransformerSyntax
	with ContextMatcherSyntax {

	object TransformerSyntax extends TransformerSyntax
	object ContextMatcherSyntax extends ContextMatcherSyntax
}
