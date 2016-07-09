package io.dylemma

import io.dylemma.xsp.syntax._

package object xsp
	extends TransformerSyntax
	with ContextMatcherSyntax {

	object TransformerSyntax extends TransformerSyntax
	object ContextMatcherSyntax extends ContextMatcherSyntax
}
