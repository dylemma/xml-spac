package xsp

import javax.xml.stream.events.{StartElement, XMLEvent}

import xsp.ExampleConsumers._
import xsp.handlers.{ContextMatcher, XMLStackHandler}

object Main1 extends App {
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
	val exampleParser = Parser.fromConsumer(ToList())
	object ExampleConsumer extends Consumer[XMLEvent, Unit] {
		def makeHandler(): Handler[XMLEvent, Unit] = {
			new XMLStackHandler(DivMatcher, exampleParser)
		}
	}

	val xmlEvents = XMLEvents(rawXML)
	val result = xmlEvents feedTo ExampleConsumer
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
