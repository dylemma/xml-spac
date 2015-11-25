package io.dylemma.xml

import javax.xml.namespace.QName
import javax.xml.stream.events.XMLEvent
import scala.concurrent.ExecutionContext
import scala.language.implicitConversions

import io.dylemma.xml.IterateeHelpers._
import io.dylemma.xml.Result._
import play.api.libs.iteratee.{ Done, Iteratee }

/**
 * Created by dylan on 10/10/2015.
 */
object ParsingDSL extends ParserCombinerOps with ChainParserOps with MapROps {

	object Text {
		object asList
		object asOption
	}

	implicit class SplitterTextSyntax[A](splitter: Splitter[A]) {
		def %(t: Text.type) = splitter.textConcat
		def %(t: Text.asOption.type) = splitter.textOption
		def %(t: Text.asList.type) = splitter.textList
	}

	/** Base context matcher that will match all contexts without
		* actually consuming any of the tag stack
		*/
	object Root extends ListMatcher[Unit] {
		def apply(inputs: List[OpenTag]) = Success(() -> inputs)
	}
	/** Tag matcher that always matches
		*/
	val * = Matcher.predicate{ _ => true }

	def tag(qname: QName) = Matcher.predicate{ _.name == qname }
	def tag(name: String) = Matcher.predicate { _.name.getLocalPart == name }
	implicit def stringToTagMatcher(name: String): Matcher[Unit] = tag(name)

	def attr(qname: QName) = Matcher{ _.attrs get qname }
	def attr(name: String): Matcher[String] = attr(new QName(name))

	def inContext[C]: ParserForContext[C, C] = new ParserForContext[C, C] {
		override def toIteratee(context: C)(implicit ec: ExecutionContext): Iteratee[XMLEvent, Result[C]] = {
			Iteratee.skipToEof.map(_ => Success(context))
		}
	}

}
