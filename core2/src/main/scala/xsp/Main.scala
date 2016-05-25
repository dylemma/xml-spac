package xsp

import syntax._

object Main3 extends App {
	val rawXML =
		s"""<foo>
			 |  <Bar>some bars</Bar>
			 |</foo>
		 """.stripMargin

	import ContextMatcherSyntax._
	import TransformerSyntax._

	// This should throw an exception because `parseFirst` does not
	// receive the "bar" element while in the "Bar" context.
	// This main is here basically to test that the exception is helpful
	val barParser = Splitter("bar").through(Parser.forText).parseFirst
	val parser = Splitter("foo" \ "Bar").through(barParser).parseFirst
	println(parser.parse(rawXML).get)
}

object Main2 extends App {
	val rawXML =
		s"""<dl>
			 |  <dt>Foo</dt>
			 |  <dd>Something fooey</dd>
			 |  <dd>But not bar-y</dd>
			 |  <dt>Bar</dt>
			 |  <dd>Something bar-y</dd>
			 |  <dd>A place to drink</dd>
			 |</dl>
		 """.stripMargin

	sealed trait Item
	case class Term(text: String) extends Item
	case class Description(text: String) extends Item

	val itemParser = Parser.choose[String] {
		case "dt" => Parser.forText map Term
		case "dd" => Parser.forText map Description
	}

	val itemTransformer = Splitter("dl" \ extractElemName) through itemParser
	val consumer = itemTransformer.consumeToList
	val result = XMLEvents(rawXML) feedTo consumer
	println(result)

	case class Foo(i: Int, s: String)
	object Foo
}

object Main1 extends App {
	// uncomment the following line to do a bunch of println's in the handler:
	// debug.enabled.set(true)

	val rawXML =
		s"""<body>
			 |  <h1>Hello</h1>
			 |  <div>Hi there</div>
			 |  <p>
			 |    Here is some text
			 |  </p>
			 |  <input type="text"/>
			 |  <div style="float:right;">Whoa</div>
			 |  <div>Another one!</div>
			 |</body>
		 """.stripMargin

	val splitter = Splitter(extractElemName \ "div")
	val innerParser = Parser.compound(
		Parser.forContext[String] ->
		Parser.forText ->
		Parser.forOptionalAttribute("style")
	)
	val parser = (splitter through innerParser).parseToList
	val result = parser parse rawXML
	println(result)
}

object Main0 extends App {

	val inputs = 1 to 100

	import Transformer.{Filter, Take}
	import Consumer.ToList
	val consumer = Filter[Int](_ % 3 == 0) >> Take[Int](5) >> ToList[Int]()

	val foreverIterator = new Iterator[Int] with AutoCloseable {
		var count = 0
		def hasNext = true
		def next(): Int = { count += 1; count }
		def close(): Unit = println("closed it!")
	}

//	val result = consumer.consume(1 to 100)
	val result = consumer consume foreverIterator
	println(s"result: $result")
}
