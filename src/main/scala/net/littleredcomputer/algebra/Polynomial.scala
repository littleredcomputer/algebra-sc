/**
  * Created by colin on 6/27/16.
  */

package net.littleredcomputer.algebra

import scala.annotation.tailrec

case class Polynomial[R] private (terms: List[Term[R]]) (implicit R: Ring[R]) {
  // the monomials of a polynomial must all have the same arity.
  private def computeArity = {
    val arities = (terms map (_.monomial.arity)).distinct
    require(arities.length <= 1, "All monomials of a polynomial must have the same arity")
    if (arities.isEmpty) 0 else arities.head
  }
  lazy val arity: Int = computeArity
  lazy val degree: Int = if (terms.isEmpty) -1 else (terms map (_.monomial.degree)).max
  // This implementation doesn't take advantage of the sorted nature
  // of input monomial lists.
  private def k(r: R) = Term(r, Monomial(Seq.fill(arity)(0)))
  def +(y: Polynomial[R]) = Polynomial.make(terms ++ y.terms)
  def +(y: Term[R]) = Polynomial.make(y :: terms)
  def +(y: R) = Polynomial.make(k(y) :: terms)
  def -(y: Polynomial[R]) = this + -y
  def -(y: Term[R]) = Polynomial.make(-y :: terms)
  def -(y: R) = this + R.unary_-(y)
  def *(y: Polynomial[R]) = Polynomial.make(for { x <- terms; y <- y.terms } yield x * y)
  def *(y: Term[R]) = Polynomial.make(for { t <- terms } yield t * y)
  def *(y: R) = map(c => R.*(c, y))
  def map[S](f: R => S) (implicit S: Ring[S]) = Polynomial.make[S](terms map (_ map f))
  def unary_- = map(R.unary_-)
  def isZero = terms.isEmpty
  def leadingTerm = terms.head
  def divide(ys: Seq[Polynomial[R]]) = {
    // Cox, Little & O'Shea "Ideals, Varieties and Algorithms 2.3 Theorem 3
    val ysi = ys.zipWithIndex
    @tailrec def step(p: Polynomial[R], qs: List[List[Term[R]]], remainder: List[Term[R]]): (List[Polynomial[R]], Polynomial[R]) = {
      @tailrec def findDivisor(ysi: Seq[(Polynomial[R], Int)]): Option[(Term[R], Int)] = ysi match {
        case (d, i) :: ds => p.leadingTerm /? d.leadingTerm match {
          case Some(divisor) => Some(divisor, i)
          case None => findDivisor(ds)
        }
        case Nil => None
      }
      if (p.isZero) (qs map Polynomial.make[R], Polynomial.make(remainder)) else findDivisor(ysi) match {
        case Some((d, i)) => step(p - ys(i) * d, qs.updated(i, d :: qs(i)), remainder)
        case None => step(p - p.leadingTerm, qs, p.leadingTerm :: remainder)
      }
    }
    step(this, List.fill(ys.length)(List()), List())
  }
  def divide(y: Polynomial[R]): (Polynomial[R], Polynomial[R]) = {
    // The CLO algorithm above, simplified for a single divisor.
    @tailrec def step(p: Polynomial[R], quotient: List[Term[R]], remainder: List[Term[R]]): (Polynomial[R], Polynomial[R]) = {
      if (p.isZero) (Polynomial.make(quotient), Polynomial.make(remainder)) else {
        p.leadingTerm /? y.leadingTerm match {
          case Some(q) => step(p - Polynomial(List(q)) * y, q :: quotient, remainder)
          case None => step(p - Polynomial(List(p.leadingTerm)), quotient, p.leadingTerm :: remainder)
        }
      }
    }
    step(this, List(), List())
  }
  def divide(y: Term[R]): (Polynomial[R], Polynomial[R]) = divide(Polynomial(List(y)))
  def pseudoRemainder(y: Polynomial[R]): (Polynomial[R], Int) = {
    require(!y.isZero)
    require(arity == 1 && y.arity == 1)
    val vn = y.leadingTerm
    val n = vn.monomial.degree
    @tailrec def step(remainder: Polynomial[R], d: Int): (Polynomial[R], Int) = {
      val m = remainder.degree
      if (m < n) (remainder, d)
      else step (remainder * vn.coefficient - (y * Term(remainder.leadingTerm.coefficient, Monomial(List(m-n)))), d+1)
    }
    step(this, 0)
  }
  // We have tolerated a shady approach to division, but now we should consider
  // whether we want to introduce subclassing based upon whether the base rings
  // are Euclidean. If we don't do it now, the code risks becoming less principled.
  def S(y: Polynomial[R]) = {
    val xGamma = Term(R.one, leadingTerm.monomial lcm y.leadingTerm.monomial)
    ((xGamma * this) divide this.leadingTerm)._1 - ((xGamma * y) divide y.leadingTerm)._1
  }
  def lower(implicit Rx: Ring[Polynomial[R]]) = {
    Polynomial.make((for ((x, qs) <- terms groupBy (_.monomial.exponents.head))
      yield Term(Polynomial.make(qs map {_.mapx (_.tail)}), Monomial(List(x)))
    ).toList)
  }
  // Are we in trouble? How do we type evaluate so that
  // given a p in P[P[R]] and an x in R, we get a P[R]?
  // We would need a scalar multiplication operation
  // defined on R x P[R} -> P[R} to get this to work.
  // This suggests that the type of the evaluation variable
  // is  a completely different type and we need some kind
  // of implicit or combinator to represent this vector
  // space operation.
  def evaluate(x: R): R = {
    require(arity == 1)
    (R.zero /: terms) {
      case (sum, Term(c, m)) => R.+(sum, R.*(c, R.^(x, m.exponents.head)))
    }
  }
}

object Polynomial {
  def make[R](ms: Seq[Term[R]]) (implicit R: Ring[R]) = {
    val terms = for {
      (xs, cs) <- ms groupBy (_.monomial.exponents)
      c = (R.zero /: cs)((sum, c) => R.+(sum, c.coefficient))
      if c != R.zero
    } yield Term(c, Monomial(xs))
    Polynomial(terms.toList.sortBy(_.monomial)(Monomial.Ordering.GrLex))
  }
  def makeDenseUnivariate[R](cs: Seq[R]) (implicit R: Ring[R]): Polynomial[R] = {
    Polynomial.make[R](cs.zipWithIndex map {case (c, i) => Term[R](c, Monomial(List(i)))})
  }
  // experiment with variance: why can't a Polynomial[Nothing] serve as a zero element?
  def zero[T]() (implicit R: Ring[T]) = make[T](List())
}
