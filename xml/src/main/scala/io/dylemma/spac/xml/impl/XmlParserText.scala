package io.dylemma.spac
package xml
package impl

object XmlParserText extends XmlParser[String] {
	def newHandler = new Handler(new StringBuilder)

	class Handler(sb: StringBuilder) extends Parser.Handler[XmlEvent, String] {
		def step(in: XmlEvent) = {
			for (e <- in.asText) sb append e.value
			Right(this)
		}
		def finish() = sb.result()
	}
}
