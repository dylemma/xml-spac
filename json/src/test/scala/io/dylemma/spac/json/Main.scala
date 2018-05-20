package io.dylemma.spac.json

import io.dylemma.spac.Consumer

object Main {
	def main(args: Array[String]): Unit = {
		val consumer = Consumer.foreach[JsonEvent](println)
		val rawJson = """{ "hello": [1, 2, 3], "world": null }"""
		consumer.consume(rawJson)
		println("<done>")
	}
}
