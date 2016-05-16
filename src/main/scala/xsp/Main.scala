package xsp

import javax.xml.stream.events.{StartElement, XMLEvent}

import xsp.ExampleConsumers._

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

	import ContextMatcherSyntax._
	import ChainParserSyntax._
	import ChainSyntax._

	val splitter = Splitter(* / "div")
	val innerParser = (Parser.forText ~ Parser.forOptionalAttribute("style")).tupled
	val consumer = splitter through innerParser andThen ToList[(String, Option[String])]

	val result = XMLEvents(rawXML) feedTo consumer
	println(result)
}

object Main0 extends App {

	val inputs = 1 to 100

	val consumer = Filter[Int](_ % 3 == 0) >> Take[Int](5) >> ToList[Int]

	implicit class ConsumerWithConsume[In, Out](consumer: Consumer[In, Out]) {
		def consume(source: Iterable[In]): Out = {
			var res: Option[Out] = None
			val handler = consumer.makeHandler()
			val itr = source.iterator
			if(itr.hasNext){
				while(!handler.isFinished && itr.hasNext){
					val input = itr.next()
					res = handler.handleInput(input)
				}
				res getOrElse handler.handleEnd()
			} else {
				handler.handleEnd()
			}
		}
	}

	val result = consumer.consume(1 to 100)
	println(s"result: $result")
}
