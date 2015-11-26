package io.dylemma.xml.example

import scala.concurrent.Future

import io.dylemma.xml.Parser.MultiplexedParser
import io.dylemma.xml.Result.Error
import io.dylemma.xml.Result._
import io.dylemma.xml._
import ParsingDSL._
import play.api.libs.iteratee.Enumeratee.CheckDone
import play.api.libs.iteratee.{Iteratee, Enumeratee}
import play.api.libs.iteratee.Execution.Implicits.trampoline

/**
 * Created by dylan on 11/24/2015.
 */
object DefinitionList extends App {

	val rawXml = """<multimap>
		| <entrylist>
		|   <key>hello</key>
		|   <value>5</value>
		|   <value>2</value>
		|   <key>goodbye</key>
		|   <key>floopy</key>
		|   <value>7</value>
		|   <key>orange</key>
		|   <value>3</value>
		|   <value>1</value>
		|   <value>5</value>
		| </entrylist>
		| <anotherThing>Hello</anotherThing>
		|</multimap>""".stripMargin

	val keyOrValueParser = new MultiplexedParser[String, Either[String, Int]]({
		case "key" => (* % Text).map(Left(_))
		case "value" => (* % Text).map(_.toInt).flatMap{
			case 4 => Error(new Exception("I don't like 4's"))
			case i => Success(i)
		}.map(Right(_))
	})

	case class Entry(key: String, values: List[Int])

	val firstPart = Root / "multimap" / ("key" | "value").extractName through keyOrValueParser

	def collectSuccesses[T] = Enumeratee.collect[Result[T]]{ case Success(t) => t }
	def collectErrors[T] = Enumeratee.collect[Result[T]]{ case e: Error => e }

//	def recovery[T] = Enumeratee.recover[T] {
//		case (err, input) => println(s"recovering $err at $input")
//	}

	type KV = Either[String, Int]
	type RKV = Result[KV]

//	def wrapIterError[A, B](iter: Iteratee[A, Result[B]]): Iteratee[Result[A], Result[B]] = {
//		// create a new iteratee that takes Result as an input
//		// passes values from Success to the `iter`, ignoring Empty,
//		// but if an Error is found, use a Done iteratee that returns the error
//
//		// maybe this method should be an enumeratee instead?
//		???
//	}

//	class FeedOrErrorEnumeratee[A] extends Enumeratee[Result[A], A]{
//		def applyOn[B](inner: Iteratee[A, B]): Iteratee[Result[A], Iteratee[A, B]] = ???
//	}

//	def wrapIterError2[From, To] = new CheckDone[Result[From], From] {
//		def step[A](k: K[From, A]): K[Result[From], Iteratee[From, A]] = {
//
//			case Input.El(Success(e)) => Iteratee.flatten(Future{
//				new CheckDone[Result[From], From]{ def continue[A](k: K[From, A]) = Cont(step(k)) } &> k(Input.El(e))
//			})
//
//			case Input.El(Empty) | Input.Empty =>
//				new CheckDone[Result[From], From] { def continue[A](k: K[From, A]) = Cont(step(k)) } &> k(Input.Empty)
//
//			case Input.El(e: Error) => Iteratee.flatten(Future{
//				??? // I dunno what to do here
//			})
//
//			case Input.EOF => Done(Cont(k), Input.EOF)
//		}
//
//		def continue[A](k: K[From, A]) = Cont(step(k))
//	}

	/*
	I want an Enumeratee[Result[A], X]
	that will take in Success results and pass them to a Parser[A]
	and ignore any Empty results
	but if an Error result is found, cause the underlying parser to emit an error
	 */
	val transformer = Enumeratee.zipWith(
		/*
		The problem here is that if there is no error, Iteratee.head will
		end up consuming the whole stream, so the actual consumer won't get
		to repeat on the remaining actual results.
		 */
		collectErrors[KV] &>> Iteratee.head,
		collectSuccesses[KV] &>> (for {
				_ <- Enumeratee.takeWhile[KV](_.isRight) &>> Iteratee.ignore
				key <- Iteratee.head[KV].map(_.flatMap(_.left.toOption))
				values <- Enumeratee.takeWhile[KV](_.isRight) ><> Enumeratee.map(_.right.get) &>> Iteratee.getChunks
			} yield {
				Result.fromOption(key) map {Entry(_, values)}
			})
	){(errOpt, actualResult) => errOpt getOrElse actualResult}

	val enumerateEntries = Enumeratee.grouped[KV]{
		for {
			_ <- Enumeratee.takeWhile[KV](_.isRight) &>> Iteratee.ignore
			key <- Iteratee.head[KV].map(_.flatMap(_.left.toOption))
			values <- Enumeratee.takeWhile[KV](_.isRight) ><> Enumeratee.map(_.right.get) &>> Iteratee.getChunks
		} yield {
			Result.fromOption(key) map { Entry(_, values) }
		}
	}

//	val secondPart = firstPart andThenR (recovery ><> takeSuccess ><> enumerateEntries)
//	val secondPart = firstPart andThenR (Enumeratee.grouped(transformer))

	def innerResultCheck(f: KV => Boolean): (RKV=>Boolean) = {
		case Success(kv) => f(kv)
		case _ => true
	}

	val floop = Enumeratee.grouped[RKV] {
		for {
			_ <- Enumeratee.takeWhile[RKV](innerResultCheck(_.isRight)) &>> Iteratee.ignore
			key <- Iteratee.head[RKV]//.map(_.map(_.collect{ case Left(k) => k }))
			values <- Enumeratee.takeWhile(innerResultCheck(_.isRight)) &>> Iteratee.getChunks
		} yield {
			val keyResult = Result.fromOption(key).flatten.collect{ case Left(k) => k }
			val valuesResult = Result.list(values.map(_ collect { case Right(v) => v }))
			println(s" -- Key = $key")
			println(s" -- Values = $values")
			for {
				key <- keyResult
				values <- valuesResult
			} yield Entry(key, values)
		}
	}

	case class FoldEmitState(
		accumKey: Option[String] = None,
		accumValues: List[Int] = Nil,
		emit: Option[Result[Entry]] = None
	)
//	type State = (Option[String], List[Int])
//	type Emit = Option[Entry]
//	Enumeratee.scanLeft[RKV].apply[(State, Emit)](None -> Nil -> None){ (accum, nextResult) =>
//		???
//	}
	val scanner = Enumeratee.scanLeft[RKV].apply(FoldEmitState()) { (state, nextResult) =>
		nextResult match {
			case Empty => state.copy(emit = None)
			case e: Error => FoldEmitState(None, Nil, Some(e))
			case Success(kv) => kv match {
				case Left(key) =>
					state.accumKey match {
						// starting a new entry
						case None => FoldEmitState(Some(key))
						// replacing an old entry
						case Some(oldKey) =>
							val toEmit = Entry(oldKey, state.accumValues)
							FoldEmitState(Some(key), Nil, Some(Success(toEmit)))
					}
				case Right(value) =>
					state.accumKey match {
						// no key to put the values with
						case None => state.copy(emit = None)
						// add the value to the accumValues
						case k =>
							FoldEmitState(k, state.accumValues :+ value, None)
					}
			}
		}
	} ><> Enumeratee.map{ s => println(s" -- intermediate state: $s"); s.emit }
	// ><> Enumeratee.collect{ case FoldEmitState(_, _, Some(emit)) => emit } ><> Enumeratee.takeWhile(!_.isError)

	case class TakeUntilState(
		emit: Option[Result[Entry]] = None,
		numErrorsEmitted: Int = 0,
		isEmittingError: Boolean = false
	)


	val floop2 = Enumeratee.scanLeft[Option[Result[Entry]]](TakeUntilState()){ (state, nextEmit) =>
		val incErrorCount = if(state.isEmittingError) 1 else 0
		val isError = nextEmit.exists(_.isError)
		TakeUntilState(nextEmit, state.numErrorsEmitted + incErrorCount, isError)
	}

	val combinedFlooper = scanner ><> floop2
		.compose(Enumeratee.takeWhile(_.numErrorsEmitted < 1))
		.compose(Enumeratee.collect{ case TakeUntilState(Some(emit), _, _) => emit })

//	type RKV = Result[Either[String, Int]]
//	val enumerateVersion2 = Enumeratee.grouped[RKV]{
//		for {
//			_ <- Enumeratee.takeWhile[RKV]{
//				case Success(Left(_)) => false
//				case _ => true
//			} &>> Iteratee.ignore
//
//		}
//	}

	val multimapParser = (
		(* / "entrylist" / ("key" | "value").extractName through keyOrValueParser transformWith combinedFlooper).parseList &
		(* / "anotherThing" % Text).map{s => println("Got anotherThing: $s"); s}
	).tupled

	val mainParser = (Root / "multimap").through(multimapParser) foreachResult println
//	val secondPart = firstPart andThen wrapEnumerateeForResults(enumerateEntries)
//	val secondPart = firstPart transformWith { _ => combinedFlooper }
//		Enumeratee.takeWhile[RKV](!_.isError) ><> Enumeratee.take(1) ><> floop
//		Enumeratee.breakE[RKV](_.isError) ><> floop
//	}

	mainParser parse rawXml
	/*parseWith { implicit ec =>
		type KV = Either[String, Int]
		val getKey = Enumeratee.take(1)

		Enumeratee.grouped[Either[String, Int]].apply[Entry] {
			for {
				key <- Iteratee.head[KV].map(_.get.left.get)
				values <- Enumeratee.takeWhile[KV](_.isRight) ><> Enumeratee.map(_.right.get) &>> Iteratee.getChunks
			} yield {
				Entry(key, values)
			}
		} &>> Iteratee.getChunks
	}*/
}
