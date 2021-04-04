package io.dylemma.spac
package impl

import org.tpolecat.typename.TypeName

class ParserFirst[In: TypeName] extends Parser.Stateless[In, In] {
	def step(in: In) = Left(in)
	def finish(): In = throw SpacException.missingFirst[In]
	override def toString = s"Parser.first[${implicitly[TypeName[In]].value}]"
}
