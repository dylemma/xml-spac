package xsp

object Main extends App {

	val inputs = 1 to 100

	import ExampleConsumers._
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
