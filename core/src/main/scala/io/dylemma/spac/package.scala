package io.dylemma

import io.dylemma.spac.syntax._

package object spac
	extends TransformerSyntax
	with ConsumerSyntax
	with ContextMatcherSyntax
	with FunctorSyntax
{
	object TransformerSyntax extends TransformerSyntax
	object ConsumerSyntax extends ConsumerSyntax
	object ContextMatcherSyntax extends ContextMatcherSyntax
	object FunctorSyntax extends FunctorSyntax
}
