package xsp

import javax.xml.stream.events.{StartElement, XMLEvent}

import xsp.ExampleConsumers._
import xsp.handlers.XMLContextSplitter

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
			 |</body>
		 """.stripMargin

	object DivMatcher extends ContextMatcher[Unit] {
		def matchContext(stack: Array[StartElement], offset: Int, length: Int) = {
			if(length >= 2 && stack(offset + 1).getName.getLocalPart == "div") Result.Success.unit
			else Result.Empty
		}
	}
	val splitter = new XMLContextSplitter(DivMatcher)
	val innerParser = Parser.fromConsumer(/*Take[XMLEvent](2) >> */ToList[XMLEvent]())
	val consumer = splitter through innerParser andThen ToList[List[XMLEvent]]

	val xmlEvents = XMLEvents(rawXML)
	val result = xmlEvents feedTo consumer
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
