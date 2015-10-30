//package io.dylemma.xml.iteratee
//
//import io.dylemma.xml.event.Name
//import io.dylemma.xml.iteratee.IterateeHelpers.OpenTag
//
///**
// * Created by dylan on 10/18/2015.
// */
//object PathMatching {
//
//	sealed trait SegmentMatch {
//		def matches(tag: OpenTag): Boolean
//	}
//
//	case class ExactSegmentMatch(s: String) extends SegmentMatch {
//		def matches(tag: OpenTag) = tag.name match {
//			case Name(`s`) => true
//			case _ => false
//		}
//	}
//
//	case object WildcardSegmentMatch extends SegmentMatch {
//		def matches(tag: OpenTag) = true
//	}
//
//}
