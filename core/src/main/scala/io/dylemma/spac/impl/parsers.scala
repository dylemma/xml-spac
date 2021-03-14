package io.dylemma.spac.impl

import cats.data.Chain
import cats.implicits._
import cats.{Applicative, Functor, Monad}
import io.dylemma.spac._

import scala.collection.mutable

class ParserPure[F[+_] : Applicative, Out](value: Out) extends Parser[F, Any, Out] {
	def step(in: Any): F[Either[Out, Parser[F, Any, Out]]] = Left(value).pure[F]
	def finish: F[Out] = value.pure[F]
	override def toString = s"Parser.pure($value)"
}

class ParserNamed[F[+_] : Functor, In, Out](name: String, p: Parser[F, In, Out]) extends Parser[F, In, Out] {
	override def toString = name
	def step(in: In): F[Either[Out, Parser[F, In, Out]]] = p.step(in).map {
		case Left(out) => Left(out)
		case Right(`p`) => Right(this)
		case Right(cont) => Right(new ParserNamed(name, cont))
	}
	def finish: F[Out] = p.finish
}

class ParserAsTransformer[F[+_], -In, +Out](parser: Parser[F, In, Out])(implicit F: Functor[F]) extends Transformer[F, In, Out] {
	def step(in: In) = F.map(parser.step(in)) {
		case Left(out) => Chain.one(out) -> None
		case Right(nextParser) =>
			val nextTransform = if (nextParser eq parser) this else new ParserAsTransformer(nextParser)
			Chain.nil -> Some(nextTransform)
	}
	def finish = F.map(parser.finish) { Chain.one }
}

class ParseFirstOpt[F[+_], In](implicit F: Applicative[F]) extends Parser[F, In, Option[In]] {
	def step(in: In): F[Either[Option[In], Parser[F, In, Option[In]]]] = F.pure(Left(Some(in)))
	def finish: F[Option[In]] = F.pure(None)
}

class ParserOptOrElse[F[+_], -In, +Out](parser: Parser[F, In, Option[Out]], resultIfNone: F[Out])(implicit F: Monad[F]) extends Parser[F, In, Out] {
	def step(in: In): F[Either[Out, Parser[F, In, Out]]] = F.flatMap(parser.step(in)) {
		case Left(Some(out)) => F.pure(Left(out))
		case Left(None) => F.map(resultIfNone)(Left(_))
		case Right(`parser`) => F.pure(Right(this))
		case Right(nextParser) => F.pure(Right(new ParserOptOrElse(nextParser, resultIfNone)))
	}
	def finish: F[Out] = F.flatMap(parser.finish) {
		case Some(out) => F.pure(out)
		case None => resultIfNone
	}
}

class ParserFind[F[+_], In](predicate: In => Boolean)(implicit F: Applicative[F]) extends Parser[F, In, Option[In]] {
	def step(_in: In) = F.map(F.pure(_in)) { in =>
		if (predicate(in)) Left(Some(in))
		else Right(this)
	}
	def finish = F.pure(None)
}

class ParserFindEval[F[+_] : Monad, In](predicate: In => F[Boolean]) extends Parser[F, In, Option[In]] {
	def step(_in: In) = _in.pure[F].flatMap { in =>
		predicate(in).map {
			case true => Left(Some(in))
			case false => Right(this)
		}
	}
	def finish = None.pure[F]
}

class MappedParser[F[+_], In, Out, Out2](inner: Parser[F, In, Out], f: Out => Out2)(implicit F: Functor[F]) extends Parser[F, In, Out2] {
	def step(in: In): F[Either[Out2, Parser[F, In, Out2]]] = F.map(inner.step(in)) {
		case Left(out) => Left(f(out))
		case Right(`inner`) => Right(this)
		case Right(nextInner) => Right(new MappedParser(nextInner, f))
	}
	def finish: F[Out2] = F.map(inner.finish)(f)
}

class ParserFold[F[+_], In, Out](state: Out, fold: (Out, In) => Out)(implicit F: Applicative[F]) extends Parser[F, In, Out] {
	def step(in: In): F[Either[Out, Parser[F, In, Out]]] = in.pure[F].map { x => Right(new ParserFold(fold(state, x), fold)) }
	def finish: F[Out] = F.pure(state)
}

class ParserImpureBuilder[F[+_], In, To](val builder: mutable.Builder[In, To])(implicit F: Applicative[F]) extends Parser[F, In, To] {
	def step(in: In) = F.pure(this).map { self =>
		self.builder += in
		Right(self)
	}
	def finish: F[To] = F.pure(this).map { _.builder.result() }
}

class ParserFoldEval[F[+_] : Monad, In, Out](state: Out, fold: (Out, In) => F[Out]) extends Parser[F, In, Out] {
	def step(in: In) = in.pure[F].flatMap { fold(state, _) }.map { nextState => Right(new ParserFoldEval(nextState, fold)) }
	def finish = state.pure[F]
}

class ParserEval[F[+_] : Monad, In, Out](init: F[Parser[F, In, Out]]) extends Parser[F, In, Out] {
	def step(in: In) = init.flatMap(_ step in)
	def finish: F[Out] = init.flatMap(_.finish)
}

class ParserCompoundN[F[+_] : Monad, In, Out](
	private val unfinished: List[(Int, Parser[F, In, Any])],
	private val finished: Map[Int, Any],
	private val assemble: (Int => Any) => Out
) extends Parser[F, In, Out] {

	override def toString = s"ParserCompoundN(unfinished = $unfinished, finished = $finished)"

	/** For debug/testing purposes,
	  * this combines the `unfinished` and `finished` collections
	  * in an array with each entry at its corresponding index,
	  * representing unfinished parsers as Rights and finished results as Lefts.
	  * Also, since Applicative / product builds things up from right to left,
	  * the parsers at the "end" end up having the lowest number indexes,
	  * so we reverse the output so each parser/result is located at the
	  * index corresponding to that parser's position in the source that constructed this parser.
	  */
	def inspect: IndexedSeq[Any Either Parser[F, In, Any]] = {
		val size = unfinished.size + finished.size
		val out = new Array[Any Either Parser[F, In, Any]](size)
		for ((i, p) <- unfinished) out(size - i - 1) = Right(p)
		for ((i, a) <- finished) out(size - i - 1) = Left(a)
		out.toVector
	}

	def finish: F[Out] = {
		unfinished.map { case (i, parser) =>
			parser.finish.map(i -> _)
		}.sequence.map { justFinished =>
			assemble(lazyConcatMap(finished, justFinished))
		}
	}
	def step(in: In): F[Either[Out, Parser[F, In, Out]]] = {
		unfinished.partitionEitherM { case (i, parser) =>
			parser.step(in).map {
				case Left(out) => Left(i -> out)
				case Right(cont) => Right(i -> cont)
			}
		}.map { case (addedFinished, newUnfinished) =>
			if (newUnfinished.isEmpty) {
				// all inner parsers have finished, so we can produce a result
				Left(assemble(lazyConcatMap(finished, addedFinished)))
			} else {
				// some inner parsers are still running, so continue with the remaining unfinished parsers
				Right(new ParserCompoundN(newUnfinished, lazyConcatMap(finished, addedFinished), assemble))
			}
		}
	}

	/** Optimization for `m1 ++ m2`, with the expectation that `m2` will often be empty.
	  * Since Map's `++` operator seems to be O(N+M),
	  * we avoid the cost of re-building an identical copy of either m1 or m2
	  */
	@inline def lazyConcatMap[A, B](m1: Map[A, B], m2: List[(A, B)]): Map[A, B] = {
		if (m2.isEmpty) m1
		else if (m1.isEmpty) m2.toMap
		else m1 ++ m2
	}

	/** Optimization related to Parser's `Applicative`.
	  * This function represents the `product` operation where the LHS parser is also a "compound" parser.
	  * The underlying finished/unfinished/assemble values from this parser and `fa` will be merged,
	  * returning a new "compound" parser.
	  * The general goal is to "flatten" the hierarchy created by Applicative's ap/map operations
	  * so we end up having fewer instantiations while the parser is running.
	  */
	def compoundProductWithLhs[A](fa: ParserCompoundN[F, In, A]): ParserCompoundN[F, In, (A, Out)] = {
		val offset = unfinished.size + finished.size
		val faUnfinished = fa.unfinished.map { case (i, p) => (i + offset, p) }
		val faFinished = fa.finished.map { case (i, r) => (i + offset, r) }
		val combinedAssemble = (get: Int => Any) => {
			val out = assemble(get)
			val a = fa.assemble(i => get(i + offset))
			a -> out
		}
		new ParserCompoundN(faUnfinished ++ unfinished, finished ++ faFinished, combinedAssemble)
	}

	/** Optimization related to Parser's `Applicative`.
	  * This function represents the `product` operation where the LHS parser is a non-compound parser.
	  * The LHS parser will be added as a new 'unfinished' member, to a new compound parser that will be returned.
	  * The general goal is to "flatten" the hierarchy created by Applicative's ap/map operations
	  * so we end up having fewer instantiations while the parser is running.
	  */
	def productWithLhs[A](fa: Parser[F, In, A]): ParserCompoundN[F, In, (A, Out)] = {
		val i = unfinished.size + finished.size
		new ParserCompoundN(
			(i -> fa) :: unfinished,
			finished,
			(get: Int => Any) => (get(i).asInstanceOf[A], assemble(get))
		)
	}

	/** Sort of a compromise between optimization and aesthetics, related to Parser's Applicative.
	  * If you put together something like `((p1, p2).tupled, p3).tupled`, when the `p3`
	  * gets handled by Applicative, it comes in as the `fa` for this method.
	  * Really this exists in order to satisfy the aesthetic requirement that the indexes
	  * for the underlying parsers be consistent with their order in the source code.
	  * If it weren't for that, we could just use `.productWithLhs(fa).map(_.swap).
	  */
	def productWithRhs[A](fa: Parser[F, In, A]): ParserCompoundN[F, In, (Out, A)] = {
		val _unfinished = unfinished.map { case (i, p) => (i + 1, p) }
		val _finished = finished.map { case (i, p) => (i + 1, p) }
		val _assemble = (get: Int => Any) => {
			val out = assemble(i => get(i + 1))
			val a = get(0).asInstanceOf[A]
			out -> a
		}
		new ParserCompoundN(
			(0 -> fa) :: _unfinished,
			_finished,
			_assemble
		)
	}

	/** Optimization related to Parser's `Applicative`.
	  * Since a compound parser already includes a rough analogue of the `map` function in its `assemble`,
	  * we can just wrap the `assemble` function with the mapping function `f`;
	  * the result of this wrapping persists throughout the lifetime of the returned parser.
	  * If we didn't do this, the default `.map` implementation would cause us to instantiate
	  * a new `MappedParser` at every step.
	  */
	override def map[Out2](f: Out => Out2)(implicit F: Functor[F]): Parser[F, In, Out2] = {
		new ParserCompoundN(unfinished, finished, (get: Int => Any) => f(assemble(get)))
	}
}