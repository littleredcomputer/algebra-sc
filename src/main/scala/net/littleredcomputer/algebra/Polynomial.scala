/**
  * Created by colin on 6/27/16.
  */

package net.littleredcomputer.algebra

import scala.annotation.tailrec

trait APolynomial[R] {
  def terms: List[Term[R]]
  private def computeArity = {
    val arities = (terms map (_.monomial.arity)).distinct
    require(arities.length <= 1, "All monomials of a polynomial must have the same arity")
    if (arities.isEmpty) 0 else arities.head
  }
  lazy val arity: Int = computeArity
  lazy val degree: Int = if (terms.isEmpty) -1 else (terms map (_.monomial.degree)).max
  def isZero = terms.isEmpty
  def leadingTerm = terms.head
  protected def constant(r: R) = Term(r, Monomial.unit(arity))
}

case class Polynomial[R] protected (terms: List[Term[R]]) (implicit R: EuclideanRing[R], O: Ordering[Monomial]) extends APolynomial[R] {
  // the monomials of a polynomial must all have the same arity.
  // This implementation doesn't take advantage of the sorted nature
  // of input monomial lists.

  def +(y: Polynomial[R]): Polynomial[R] = Polynomial.make(terms ++ y.terms)
  def +(y: R) = Polynomial.make(constant(y) :: terms)
  def -(y: Polynomial[R]) = this + (-y)
  def -(y: Term[R]) = Polynomial.make(Term(R.unary_-(y.coefficient), y.monomial) :: terms)
  def -(y: R) = this + R.unary_-(y)
  private def tt(x: Term[R], y: Term[R]) = Term(R.*(x.coefficient, y.coefficient), x.monomial * y.monomial)
  def *(y: Polynomial[R]) = Polynomial.make(for { t <- terms; y <- y.terms } yield tt(t, y))
  def *(y: Term[R]) = Polynomial.make(for { t <- terms } yield tt(t, y))
  def *(y: R) = map(c => R.*(c, y))

  def unit: Polynomial[R] = new Polynomial(List(Term(R.one, Monomial.unit(arity))))
  def ^(e: Int): Polynomial[R] = {
    @tailrec def step(x: Polynomial[R], e: Int, r: Polynomial[R]): Polynomial[R] = if (e == 0) r
      else if (e % 2 == 0) step (x * x, e/2, r)
      else step(x, e-1, x*r)
    step(this, e, unit)
  }
  def map[S](f: R => S) (implicit S: EuclideanRing[S]) = Polynomial.make[S](terms map (_ map f))
  def unary_- = map(R.unary_-)
  def /?(p: Term[R], q: Term[R]): Option[Term[R]] = {
    val qx = (p.monomial.exponents, q.monomial.exponents).zipped map (_ - _)
    if (qx.forall(_ >= 0)) {
      R./%(p.coefficient, q.coefficient) match {
        case (quotient, remainder) if remainder == R.zero => Some(Term[R](quotient, Monomial(qx)))
        case _ => None
      }
    } else None
  }
  def divide(ys: Seq[Polynomial[R]]) = {
    // Cox, Little & O'Shea "Ideals, Varieties and Algorithms 2.3 Theorem 3
    val ysi = ys.zipWithIndex
    @tailrec def step(p: Polynomial[R], qs: List[List[Term[R]]], remainder: List[Term[R]]): (List[Polynomial[R]], Polynomial[R]) = {
      @tailrec def findDivisor(ysi: Seq[(Polynomial[R], Int)]): Option[(Term[R], Int)] = ysi match {
        case (d, i) :: ds => /?(p.leadingTerm, d.leadingTerm) match {
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
        /?(p.leadingTerm, y.leadingTerm) match {
          case Some(q) => step(p - y * q, q :: quotient, remainder)
          case None => step(p - p.leadingTerm, quotient, p.leadingTerm :: remainder)
        }
      }
    }
    step(this, List(), List())
  }
  def divide(y: Term[R]): (Polynomial[R], Polynomial[R]) = divide(new Polynomial(List(y)))
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
    val xGamma = Polynomial(List(Term(R.one, leadingTerm.monomial lcm y.leadingTerm.monomial)))
    // The use of _1 below is what makes this wrong.
    //((xGamma * this) divide this.leadingTerm)._1 - ((xGamma * y) divide y.leadingTerm)._1
    (xGamma divide this.leadingTerm)._1 * this - (xGamma divide y.leadingTerm)._1 * y
  }
  def lower(implicit Rx: EuclideanRing[Polynomial[R]]): Polynomial[Polynomial[R]] = {
    Polynomial.make((for ((x, qs) <- terms groupBy (_.monomial.exponents.head))
      yield Term(Polynomial.make(qs map {_.mapx (_.tail)}), Monomial(List(x)))
    ).toList)
  }
  def evaluate(x: R): R = {
    require(arity == 1)
    (R.zero /: terms) {
      case (sum, Term(c, m)) => R.+(sum, R.*(c, R.^(x, m.exponents.head)))
    }
  }
  override def toString() = terms.mkString("[", " ", "]")
}



object GroebnerBasis {
  private def pairs[T](s: List[T]): List[(T, T)] = s match {
    case x :: xs => (for {y <- xs} yield (x, y)) ++ pairs(xs)
    case Nil => Nil
  }
  def Buchberger[F](fs: List[Polynomial[F]])(implicit F: Field[F]) = {
    var G = fs.toSet
    var Gprime = G.empty
    var done = false
    while (!done) {
      Gprime = G
      pairs(Gprime.toList) foreach { case ((p: Polynomial[F], q: Polynomial[F])) =>
        val (_, r) = (p S q) divide Gprime.toList
        if (!r.isZero) G += r
      }
      if (G == Gprime) done = true
    }
    G
  }
  def of[F](fs: Polynomial[F]*)(implicit F: Field[F]) = Buchberger(fs.toList)
}

object Polynomial {

  def make[R](ts: Seq[Term[R]])(implicit R: EuclideanRing[R], order: Ordering[Monomial]) = {
    val terms = for {
      (xs, cs) <- ts groupBy (_.monomial.exponents)
      c = (R.zero /: cs)((sum, c) => R.+(sum, c.coefficient))
      if c != R.zero
    } yield Term(c, Monomial(xs))
    new Polynomial(terms.toList.sortBy(_.monomial)(order))
  }
  def make[R](t: Term[R]) (implicit R: EuclideanRing[R], O: Ordering[Monomial]) = new Polynomial(List(t))
  // XXX for a univariate polynomial the ordering is not controversial?
  def makeDenseUnivariate[R](cs: Seq[R]) (implicit R: EuclideanRing[R], O: Ordering[Monomial]): Polynomial[R] = {
    Polynomial.make[R](cs.zipWithIndex map {case (c, i) => Term[R](c, Monomial(List(i)))})
  }
  // experiment with variance: why can't a Polynomial[Nothing] serve as a zero element?
  def zero[T] (implicit R: EuclideanRing[T], O: Ordering[Monomial]) = make[T](List())


  private def variables[R](arity: Int) (implicit R: EuclideanRing[R], O: Ordering[Monomial]): IndexedSeq[Polynomial[R]] = for {i <- 0 until arity} yield Polynomial(List(Term(R.one, Monomial.basis(i, arity))))
  def vars1[R](f: Polynomial[R] => Unit)(implicit R: EuclideanRing[R], O: Ordering[Monomial]) = {
    val vs = variables(1)
    f(vs(0))
  }
  def vars2[R](f: (Polynomial[R], Polynomial[R]) => Unit)(implicit R: EuclideanRing[R], O: Ordering[Monomial]) = {
    val vs = variables(2)
    f(vs(0), vs(1))
  }
  def vars3[R](f: (Polynomial[R], Polynomial[R], Polynomial[R]) => Unit)(implicit R: EuclideanRing[R], O: Ordering[Monomial]) = {
    val vs = variables(3)
    f(vs(0), vs(1), vs(2))
  }
}


