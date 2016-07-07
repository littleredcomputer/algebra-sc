/**
  * Created by colin on 6/27/16.
  */

package net.littleredcomputer.algebra

import org.apache.commons.math3.fraction.BigFraction

import scala.annotation.tailrec

case class Polynomial[R] private (ms: List[Monomial[R]]) (implicit R: Ring[R]) {
  // the monomials of a polynomial must all have the same arity.
  private def computeArity = {
    val arities = (ms map (_.exponents.length)).distinct
    require(arities.length <= 1, "All monomials of a polynomial must have the same arity")
    arities.headOption.getOrElse(0)
  }
  val arity: Int = computeArity
  // This implementation doesn't take advantage of the sorted nature
  // of input monomial lists.
  def +(y: Polynomial[R]) = Polynomial.make(ms ++ y.ms)
  def *(y: Polynomial[R]) = Polynomial.make(for {
    x <- ms
    y <- y.ms
  } yield x * y)
  def map[S](f: R => S) (implicit S: Ring[S]) = Polynomial.make[S](ms map (_ map f))
  def unary_- = map(R.unary_-)
  def -(y: Polynomial[R]) = this + (-y)
  def isZero = ms.isEmpty
  def leadingTerm = ms.head
  def divide(y: Polynomial[R]) = {
    // Cox, Little & O'Shea "Ideals, Varieties and Algorithms" 2.3 Theorem 3 (1 divisor)
    @tailrec def step(p: Polynomial[R], quotient: List[Monomial[R]], remainder: List[Monomial[R]]): (Polynomial[R], Polynomial[R]) = {
      if (p.isZero) (Polynomial.make(quotient), Polynomial.make(remainder)) else {
        p.leadingTerm /? y.leadingTerm match {
          case Some(q) => step(p - Polynomial[R](List(q)) * y, q :: quotient, remainder)
          case None => step(p - Polynomial[R](List(p.leadingTerm)), quotient, p.leadingTerm :: remainder)
        }
      }
    }
    step(this, List(), List())
  }
  // CL&O in full form. Can we get this done in functional style? That'll be a challenge.
  def divide(ys: Seq[Polynomial[R]]) = ???  // er, haven't started this yet
  def lower(implicit Rx: Ring[Polynomial[R]]) = {
    Polynomial.make((for ((x, xs) <- ms groupBy (_.exponents.head))
      yield Monomial.make(Polynomial(xs map {_ mapx (_.tail)}), List(x))
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
  def evaluate[S](x: Ring[S]) (implicit S: Ring[S]): R = {
    var s: S = S.zero
    ???
  }
}

object Polynomial {
  def make[T](ms: Seq[Monomial[T]]) (implicit R: Ring[T]) = {
    val terms = for {
      (xs, cs) <- ms groupBy (_.exponents)
      c = (R.zero /: cs)((sum, m) => R.+(sum, m.coefficient))
      if c != R.zero
    } yield Monomial(c, xs)
    Polynomial(terms.toList.sorted(Monomial.Ordering.GrLex))
  }
  // experiment with variance: why can't a Polynomial[Nothing] serve as a zero element?
  def zero[T]() (implicit R: Ring[T]) = make[T](List())
}

object MyApp extends App {

  val x = Monomial.make(1, List(1,0))
  val y = Monomial.make(1, List(0,1))
  val twox = Monomial.make(2, List(1,0))
  val twody = Monomial.make(2.1, List(0, 1))
  val xy = Monomial.make[Int](1, List(1,1))
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
  println(xyxy2x2x.arity)
  println(xyxy2x2x.degree)



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
  println("z.ms", z.ms)
  println("z.ms.head", z.ms.head)
  println("z.lower", z.lower)
  println("z.arity", z.arity)
  println("z.lower.arity", z.lower.arity)

  println("o1", Monomial.Ordering.GrLex.compare(x, xy))
  println("o2", Monomial.Ordering.GrLex.compare(xy, x))

  println(List(x,xy).sorted(Monomial.Ordering.GrLex))
  println(List(xy,x).sorted(Monomial.Ordering.GrLex))

  println("0", Polynomial.make(List(Monomial.make(0, List(1,0)))))
  println("z+z", z+z)
  println("z*z", z*z)
  println("-z", -z)
  println("-(z^2)", -(z*z))
  println("z-z", z + (- z))
  println("z-z", z - z)
  println("zz - z", z*z-z)

  // Hm. well, we're learning. This is one way to bring the
  // argument into scope. Is there a better way?

  // well, that is certainly one way! So it appears the companion
  // object is kind of privileged.
  // But why can't we leave the type parameter off?


  val pp: Polynomial[Polynomial[Int]] = Polynomial.make(
    List(
      Monomial.make(z, List(1)),
      Monomial.make(z*z, List(0))))
  println("Z", pp)

  val qx = Monomial.make[BigFraction](BigFraction.ONE_HALF, List(1, 0))
  val qy = Monomial.make[BigFraction](BigFraction.ONE_HALF, List(0, 1))
  val qxy = qx * qy
  println("qxy", qxy)
  val pxy = Polynomial[BigFraction](List(qxy))
  println("pxy", pxy)
  println("pxy2-pxy", pxy*pxy - pxy)



}
