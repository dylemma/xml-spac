package io.dylemma

import io.dylemma.spac.syntax._
import language.implicitConversions

package object spac
	extends TransformerSyntax
	with ContextMatcherSyntax
{
	object TransformerSyntax extends TransformerSyntax
	object ContextMatcherSyntax extends ContextMatcherSyntax

	type ContextualParser[-Context, +Out] = Context => Parser[Out]
	implicit def upgradeParserToAnyContextual[Out](parser: Parser[Out]): ContextualParser[Any, Out] = { _ => parser }
	implicit def getContextualParser[Out](implicit parser: Parser[Out]): ContextualParser[Any, Out] = { _ => parser }
}
