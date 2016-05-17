package xsp

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

	import ContextMatcherSyntax._
	val itemParser = Parser.choose[String] {
		case "dt" => Parser.forText map Term
		case "dd" => Parser.forText map Description
	}

	val itemTransformer = Splitter("dl" / extractElemName) through itemParser
	val consumer = itemTransformer.consumeToList
	val result = XMLEvents(rawXML) feedTo consumer
	println(result)
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

	import ContextMatcherSyntax._
	import TransformerSyntax._

	val splitter = Splitter(extractElemName / "div")
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
