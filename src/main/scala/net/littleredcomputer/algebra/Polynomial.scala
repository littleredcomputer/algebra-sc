/**
  * Created by colin on 6/27/16.
  */

package net.littleredcomputer.algebra

import scala.Function.tupled


case class Monomial[T] (coefficient: T, exponents: Vector[Int]) (implicit R: Ring[T]) {
  val arity = exponents.size
  val degree = exponents.sum
  def *(y: Monomial[T]) = {
    require(arity == y.arity)
    Monomial(R.*(coefficient, y.coefficient), (exponents zip y.exponents) map tupled {_ + _})
  }
  override def toString = "" + coefficient + "×" + exponents.toList.mkString("⋅")
}

object Monomial {
  object Ordering {
    object GrLex extends Ordering[Monomial[_]] {
      override def compare(x: Monomial[_], y: Monomial[_]): Int = y.degree - x.degree
    }
  }
}

case class Polynomial[T] private (ms: List[Monomial[T]]) (implicit R: Ring[T]) {
  // This implementation doesn't take advantage of the sorted nature
  // of input monomial lists.
  def +(y: Polynomial[T]) = Polynomial.make(ms ++ y.ms)
  def *(y: Polynomial[T]) = Polynomial.make(for {
    x <- ms
    y <- y.ms
  } yield x * y)
  def unary_- = Polynomial.make(ms map {
    case Monomial(c, es) => Monomial(R.unary_-(c), es)
  })
  def -(y: Polynomial[T]) = this + (-y)
}

object Polynomial {
  def make[T](ms: Seq[Monomial[T]]) (implicit R: Ring[T]) = {
    val terms = for {
      (es, cs) <- ms groupBy (_.exponents)
      c = (R.zero /: cs)((sum, m) => R.+(sum, m.coefficient))
      if c != R.zero
    } yield Monomial(c, es)
    Polynomial(terms.toList.sorted(Monomial.Ordering.GrLex))
  }
  // experiment with variance: why can't a Polynomial[Nothing] serve as a zero element?
  def zero[T]() (implicit R: Ring[T]) = make[T](List())
}

object MyApp extends App {

  val x = Monomial[Int](1, Vector(1,0))
  val y = Monomial(1, Vector(0,1))
  val twox = Monomial(2, Vector(1,0))
  val twody = Monomial[Double](2.1, Vector(0, 1))
  val xy = Monomial[Int](1, Vector(1,1))
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

  println("o1", Monomial.Ordering.GrLex.compare(x, xy))
  println("o2", Monomial.Ordering.GrLex.compare(xy, x))

  println(List(x,xy).sorted(Monomial.Ordering.GrLex))
  println(List(xy,x).sorted(Monomial.Ordering.GrLex))

  println("0", Polynomial.make(List(Monomial(0, Vector(1,0)))))
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
      Monomial(z, Vector(1)),
      Monomial(z*z, Vector(0))))
  println("Z", pp)


}
