package io.dylemma.spac.old.json.syntax

import io.dylemma.spac.ContextMatcher
import io.dylemma.spac.old.json._
import io.dylemma.spac.old.json.JsonEvent.{ArrayStart, ObjectStart}

import scala.language.implicitConversions

trait ContextMatcherSyntax {

	// Match an entire object

	val inObject = SingleTokenContextMatcher[Unit]({
		case ObjectStart => Some(())
		case _ => None
	}, "inObject")

	// Match an entire array

	val inArray = SingleTokenContextMatcher[Unit]({
		case ArrayStart => Some(())
		case other => None
	}, "inArray")

	// Match by field

	def bareField(name: String) = SingleTokenContextMatcher[Unit]({
		case JsonEvent.FieldStart(`name`) => Some(())
		case _ => None
	}, s"bareField($name)")

	def bareField[A](contextFromName: String => Option[A]) = SingleTokenContextMatcher[A]({
		case JsonEvent.FieldStart(name) => contextFromName(name)
		case _ => None
	}, s"bareField($contextFromName)")

	def bareFieldWhere(p: String => Boolean) = SingleTokenContextMatcher[String]({
		case JsonEvent.FieldStart(name) if p(name) => Some(name)
		case _ => None
	}, s"bareFieldWhere($p)")

	val anyBareField = SingleTokenContextMatcher[String]({
		case JsonEvent.FieldStart(name) => Some(name)
		case _ => None
	}, "anyBareField")

	// Match by array index

	def bareIndex(i: Int) = SingleTokenContextMatcher[Unit]({
		case JsonEvent.IndexStart(`i`) => Some(())
		case _ => None
	}, s"bareIndex($i)")

	def bareIndex[A](contextFromIndex: Int => Option[A]) = SingleTokenContextMatcher[A]({
		case JsonEvent.IndexStart(i) => contextFromIndex(i)
		case _ => None
	}, s"bareIndex($contextFromIndex)")

	def bareIndexWhere(p: Int => Boolean) = SingleTokenContextMatcher[Int]({
		case JsonEvent.IndexStart(i) if p(i) => Some(i)
		case _ => None
	}, s"bareIndexWhere($p)")

	val anyBareIndex = SingleTokenContextMatcher[Int]({
		case JsonEvent.IndexStart(i) => Some(i)
		case _ => None
	}, s"anyBareIndex")

	// Match by field inside an object

	def field(name: String): ContextMatcher[JsonStackElem, Unit] = inObject \ bareField(name)
	def field[A](contextFromName: String => Option[A]): ContextMatcher[JsonStackElem, A] = inObject \ bareField(contextFromName)
	def fieldWhere(p: String => Boolean): ContextMatcher[JsonStackElem, String] = inObject \ bareFieldWhere(p)
	val anyField: ContextMatcher[JsonStackElem, String] = inObject \ anyBareField

	// Match by index inside an array

	def index(i: Int): ContextMatcher[JsonStackElem, Unit] = inArray \ bareIndex(i)
	def index[A](contextFromIndex: Int => Option[A]): ContextMatcher[JsonStackElem, A] = inArray \ bareIndex(contextFromIndex)
	def indexWhere(p: Int => Boolean): ContextMatcher[JsonStackElem, Int] = inArray \ bareIndexWhere(p)
	val anyIndex: ContextMatcher[JsonStackElem, Int] = inArray \ anyBareIndex

	// Implicit conversions (String => Field) and (Int => Index)

	implicit def stringToFieldMatcher(name: String): ContextMatcher[JsonStackElem, Unit] = field(name)
	implicit def intToIndexMatcher(i: Int): ContextMatcher[JsonStackElem, Unit] = index(i)
}
