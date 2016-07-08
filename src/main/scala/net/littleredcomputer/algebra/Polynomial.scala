/**
  * Created by colin on 6/27/16.
  */

package net.littleredcomputer.algebra

import org.apache.commons.math3.fraction.BigFraction

import scala.annotation.tailrec

case class Polynomial[R] private (terms: List[Term[R]]) (implicit R: Ring[R]) {
  // the monomials of a polynomial must all have the same arity.
  private def computeArity = {
    val arities = (terms map (_.monomial.arity)).distinct
    require(arities.length <= 1, "All monomials of a polynomial must have the same arity")
    arities.headOption.getOrElse(0)
  }
  lazy val arity: Int = computeArity
  // This implementation doesn't take advantage of the sorted nature
  // of input monomial lists.
  def +(y: Polynomial[R]) = Polynomial.make(terms ++ y.terms)
  def *(y: Polynomial[R]) = Polynomial.make(for {
    x <- terms
    y <- y.terms
  } yield x * y)
  def *(y: R) = map(c => R.*(y, c))
  def map[S](f: R => S) (implicit S: Ring[S]) = Polynomial.make[S](terms map (_ map f))
  def unary_- = map(R.unary_-)
  def -(y: Polynomial[R]) = this + (-y)
  def isZero = terms.isEmpty
  def leadingTerm = terms.head
  def divide(ys: Seq[Polynomial[R]]) = {
    // Cox, Little & O'Shea "Ideals, Varieties and Algorithms 2.3 Theorem 3
    @tailrec def step(p: Polynomial[R], qs: List[List[Term[R]]], remainder: List[Term[R]]): (List[Polynomial[R]], Polynomial[R]) = {
      if (p.isZero) (qs map Polynomial.make[R], Polynomial.make(remainder)) else {
        val i = ys.indexWhere(y => (p.leadingTerm /? y.leadingTerm).isDefined)
        if (i < 0) step(p - Polynomial(List(p.leadingTerm)), qs, p.leadingTerm :: remainder)
        else {
          val y = ys(i)
          val q = (p.leadingTerm /? y.leadingTerm).get
          step(p - Polynomial(List(q)) * y, qs.updated(i, q :: qs(i)), remainder)
        }
      }
    }
    step(this, (ys map (_ => List())).toList, List())
  }
  def divide(y: Polynomial[R]) = {
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
  def lower(implicit Rx: Ring[Polynomial[R]]) = {
    Polynomial.make((for ((x, qs) <- terms groupBy (_.monomial.exponents.head))
      yield Term(Polynomial.make(qs map {_.mapm (_.tail)}), Monomial(List(x)))
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
      case (sum, Term(c, m)) => R.+(sum, R.*(c, R.expt(x, m.exponents.head)))
    }
  }
  //  def evaluate(xs: Array[R]): R = {
  //    require(arity == xs.length)
  //    (R.zero /: ts) {
  //      case (sum, Term(c, m)) =>
  //        (R.one /: m.exponents)
  //    }
  //  }
}

object Polynomial {
  def make[T](ms: Seq[Term[T]]) (implicit R: Ring[T]) = {
    val terms = for {
      (xs, cs) <- ms groupBy (_.monomial.exponents)
      c = (R.zero /: cs)((sum, c) => R.+(sum, c.coefficient))
      if c != R.zero
    } yield Term(c, Monomial(xs))
    Polynomial(terms.toList.sortBy(_.monomial)(Monomial.Ordering.GrLex))
  }
  // experiment with variance: why can't a Polynomial[Nothing] serve as a zero element?
  def zero[T]() (implicit R: Ring[T]) = make[T](List())
}

object MyApp extends App {

  val w = Polynomial.make(List(Term(1, Monomial(List(1)))))
  val one1 = Polynomial.make(List(Term(1, Monomial(List(0)))))
  val pw = w*w - (w+w) + one1
  println("pw", pw)
  println("pw@4", pw.evaluate(4))
  println("pw@-4..4", (-4 to 4) map pw.evaluate)
  println("pw@1", pw evaluate 1)
  println("pw@2", pw evaluate 2)

  val x = Term(1, Monomial(List(1,0)))
  val y = Term(1, Monomial(List(0,1)))
  val twox = Term(2, Monomial(List(1,0)))
  val twody = Term(2.1, Monomial(List(0,1)))
  val xy = Term[Int](1, Monomial(List(1,1)))
  val x_xy = x * xy
  val xyxy = xy * xy
  val xyxyx = xyxy * x
  val xyxy2x2x = xyxy * twox * twox

  println(x)
  println(xy)
  println(x_xy)
  println(xyxy)
  println(xyxyx)
  println(xyxy2x2x)
  println(xyxy2x2x.monomial.arity)
  println(xyxy2x2x.monomial.degree)

  val p = Polynomial.make(List(xy))
  println("p", p)
  val q = Polynomial.make(List(x, xy))
  println("q", q)

  val x3 = Polynomial.make(List(x,x,x))
  println("3x", x3)
  val x3y2 = Polynomial.make(List(x,y,x,y,x))
  println("3x+2y", x3y2)
  val z = Polynomial.make(List(xyxy,x,y,x,y,xyxyx,xyxy,xyxy))
  println("z", z)
  println("z.ms", z.terms)
  println("z.ms.head", z.terms.head)
  println("z.lower", z.lower)
  println("z.arity", z.arity)
  println("z.lower.arity", z.lower.arity)

  // naturally this doesn't work, because the types are off,
  // even though z.lower has arity 1. We'll have to think about this.
  // println("z.lower@1", z.lower.evaluate(1))

  println("o1", Monomial.Ordering.GrLex.compare(x.monomial, xy.monomial))
  println("o2", Monomial.Ordering.GrLex.compare(xy.monomial, x.monomial))

  println(List(x,xy).sortBy(_.monomial)(Monomial.Ordering.GrLex))
  println(List(xy,x).sortBy(_.monomial)(Monomial.Ordering.GrLex))

  println("0", Polynomial.make(List(Term(0, Monomial(List(1,0))))))
  println("z+z", z+z)
  println("z*z", z*z)
  println("-z", -z)
  println("-(z^2)", -(z*z))
  println("z-z", z + (- z))
  println("z-z", z - z)
  println("zz - z", z*z-z)

  val pp: Polynomial[Polynomial[Int]] = Polynomial.make(
    List(
      Term(z, Monomial(List(1))),
      Term(z*z, Monomial(List(0)))))
  println("Z", pp)

  val qx = Term[BigFraction](BigFraction.ONE_HALF, Monomial(List(1, 0)))
  val qy = Term[BigFraction](BigFraction.ONE_HALF, Monomial(List(0, 1)))
  val qxy = qx * qy
  println("qxy", qxy)
  val pxy = Polynomial[BigFraction](List(qxy))
  println("pxy", pxy)
  println("pxy2-pxy", pxy*pxy - pxy)



}
