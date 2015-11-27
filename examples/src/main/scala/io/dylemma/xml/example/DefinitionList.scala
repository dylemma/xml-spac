package io.dylemma.xml.example

import scala.concurrent.Future

import io.dylemma.xml.Parser.MultiplexedParser
import io.dylemma.xml.Result.Error
import io.dylemma.xml.Result._
import io.dylemma.xml._
import ParsingDSL._
import play.api.libs.iteratee.Enumeratee.CheckDone
import play.api.libs.iteratee.{Iteratee, Enumeratee, Enumerator}
import play.api.libs.iteratee.Execution.Implicits.trampoline
import play.api.libs.iteratee.Input

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
		|   <value>3</value>
		|   <value>4</value>
		|   <value>5</value>
		|   <key>orange</key>
		|   <value>3</value>
		|   <value>4</value>
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

	import IterateeHelpers.{ scanResultsWith, takeThroughFirstError}

	/* This folder takes a stream of keys and values, grouping them up into Entries.
	 * Internal state is an Option[Key] + List[Value].
	 */
	object EntryFolder extends StreamScan[(Option[String], List[Int]), Either[String, Int], Entry] {
		def init = None -> Nil
		def finish(state: (Option[String], List[Int])) = {
			Result.fromOption(state._1).map(Entry(_, state._2))
		}
		def fold(state: (Option[String], List[Int]), input: Either[String, Int]) = input match {
			case Left(key) =>
				state match {
					// emit the current state and start a new entry
					case (Some(oldKey), values) => (Some(key), Nil) -> Result{ Entry(oldKey, values) }
					// start a new entry
					case (None, values) => (Some(key), values) -> Empty
				}
			case Right(value) =>
				state match {
					// add the value to the current state
					case (Some(key), values) => (Some(key), values :+ value) -> Empty
					// ignore values that appear before a key
					case (None, values) => (None, Nil) -> Empty
				}
		}
	}


	// create an Enumeratee from the EntryFolder,
	// then make sure it stops passing through values after the first error
	val entryTransformer = scanResultsWith(EntryFolder) ><> takeThroughFirstError

	// TODO: Transformer.scanR(StreamScanner)
	// TODO: Transformer.scan(StreamScanner)
	// TODO: Transformer.takeUntil(Nth)Error
	// TODO: Transformer.takeThrough(Nth)Error

	val cleanerParser = (
		(* / "entrylist" / ("key" | "value").extractName).through(keyOrValueParser).transformWith(entryTransformer).mapR{
			entry => println(s" -- entryParser entry: $entry"); entry
		}.parseList &
		(* / "anotherThing" % Text).map{s => println(s" -- anotherThing: $s"); s }
	).tupled

	val mainParser = (Root / "multimap").through(cleanerParser) foreachResult println

	mainParser parse rawXml
}
