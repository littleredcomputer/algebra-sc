package net.littleredcomputer.algebra

/**
  * Created by colin on 7/6/16.
  */

case class Monomial private (exponents: Seq[Int]) {
  val arity = exponents.length
  val degree = exponents.sum
  def *(y: Monomial) = {
    require(arity == y.arity)
    Monomial((exponents, y.exponents).zipped map (_ + _))
  }
  def lcm(y: Monomial) = {
    require(arity == y.arity)
    Monomial((exponents, y.exponents).zipped map (_ max _))
  }
  def ^(y: Int) = Monomial(exponents map (_ * y))
  def map(f: Seq[Int] => Seq[Int]) = Monomial(f(exponents))
  override def toString = exponents.mkString("⋅")
}

object Monomial {
  object Ordering {
    implicit object Lex extends Ordering[Monomial] {
      override def compare(x: Monomial, y: Monomial) = {
        val diff = (y.exponents, x.exponents).zipped map (_-_) dropWhile (_ == 0)
        if (diff.isEmpty) 0 else diff.head
      }
    }
    implicit object GrLex extends Ordering[Monomial] {
      override def compare(x: Monomial, y: Monomial): Int = {
        val grade = y.degree - x.degree
        if (grade != 0) grade else Lex.compare(x, y)
      }
    }
  }
  def basis(i: Int, n: Int) = Monomial(for {j <- 0 until n} yield if (i == j) 1 else 0)
  def unit(n: Int) = Monomial(List.fill(n)(0))
}
