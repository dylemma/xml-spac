package io.dylemma

import io.dylemma.spac.syntax._

package object spac
	extends TransformerSyntax
	with ContextMatcherSyntax
	with FunctorSyntax
{
	object TransformerSyntax extends TransformerSyntax
	object ContextMatcherSyntax extends ContextMatcherSyntax
	object FunctorSyntax extends FunctorSyntax
}
