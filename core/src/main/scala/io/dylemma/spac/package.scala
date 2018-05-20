package io.dylemma

import io.dylemma.spac.syntax._

package object spac
	extends ConsumerSyntax
	with TransformerSyntax
{
	object ConsumerSyntax extends ConsumerSyntax
	object TransformerSyntax extends TransformerSyntax
}
