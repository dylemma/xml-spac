package io.dylemma.xml.iteratee

import javax.xml.stream.events.XMLEvent
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import io.dylemma.xml.XMLEventEnumerator
import io.dylemma.xml.event._
import play.api.libs.iteratee._

/**
 * Created by Dylan on 6/29/2015.
 */
object IterateeTesting extends App with IterateeHelpers {

	val xmlSource = """<thing>
		<a stuff="cool">
			This is some text
		</a>
		<b stuff="uncool">
			<stats>
				<stat name="asdf">Doop</stat>
				<stat name="fdsa">Goop</stat>
			</stats>
		</b>
	</thing>"""
	val xmlStream = XMLEventEnumerator(xmlSource)

	// accumulates a TagStack (list of Strings) when given XMLEvents
	object TagStackAccumulator extends StateAccumulator[List[String], XMLEvent] {
		def init = Nil
		def update(stack: List[String], event: XMLEvent) = event match {
			case StartElement(Name(tag), _) => stack :+ tag
			case EndElement(_) => stack dropRight 1
			case _ => stack
		}
	}

	// Iteratee that collects all xml Characters events, concatenating the text within
	val xmlTextCollector = Enumeratee.collect[XMLEvent] { case Characters(text) => text.trim } &>> Iteratee.consume[String]()

	def xmlAttributeCollector(attribute: String) = Enumeratee.collect[XMLEvent] {
		case StartElement(qname, attributes) => attributes get attribute
	} &>> Iteratee.head.map(_.flatten)

	// Collect the text values inside each `thing \ b \ stats \ stat` element
	val statScope = List("thing", "b", "stats", "stat")
	val statScopeFilter = { tags: List[String] => tags startsWith statScope }
	val collectStatText = subdivideOnState(TagStackAccumulator, statScopeFilter).combineWith(xmlTextCollector) &>> Iteratee.getChunks
	// (xmlAttributeCollector("stuff"))
	// (Iteratee.getChunks)
	// (Iteratee.foreach(e => println(s" > $e")))

	val aScope = List("thing", "a")
	val aScopeFilter = { tags: List[String] => tags startsWith aScope }
	val collectAStuff = subdivideOnState(TagStackAccumulator, aScopeFilter).combineWith(xmlAttributeCollector("stuff")) &>> Iteratee.head.map(_.flatten)

	val multiIteratee = Enumeratee.zip[XMLEvent, List[String], Option[String]](collectStatText, collectAStuff)

	import ParsingDSL._
	import play.api.libs.functional.syntax._
	val statTextParser = XML \ "thing" \ "b" \ "stats" \ "stat" \ Text
	val aStuffParser = XML \ "thing" \ "a" % "stuff"
	val zippedParser = Enumeratee.zip(statTextParser.toIteratee, aStuffParser.toIteratee)

	case class Stat(name: String, value: String)
	implicit val StatParser: Parser[Stat] = (
		(XML % "name") ~
		(XML \ Text)
	)(Stat.apply _)

	case class BData(stuff: String, stat: List[Stat])
	implicit val BDataParser: Parser[BData] = (
		(Elem % "stuff") ~
		(Elem \ "stats" \ "stat").asList[Stat]
	)(BData.apply _)

	val parser = (XML \ "thing" \ "b").as[BData].toIteratee
//	val parser = (
//		(XML \ "thing" \ "b" \ Text) ~
//		(XML \ "thing" \ "b" % "stuff")
//	).tupled.toIteratee

	val consumer = (XML \ "thing" \ "b").asEnumeratee(
		Enumeratee.filter[XMLEvent]{ case Characters(s) => s.trim.nonEmpty; case _ => true } &>> Iteratee.foreach(println)
	) &>> Iteratee.foreach(_ => println("=" * 20))

	// collect each text result: expect "Doop" and "Goop"
//	val resultF = xmlStream &> collectStatText |>>> Iteratee.getChunks
	val resultF = xmlStream |>>> parser

	// block on the future so the main thread doesn't exit early
	val result = Await.result(resultF, 3.seconds)
	println(s"result: $result")
}
