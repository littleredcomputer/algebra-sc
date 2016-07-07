package net.littleredcomputer.algebra

import scala.collection.mutable

/**
  * Created by colin on 7/6/16.
  */


case class Monomial[T] private (coefficient: T, exponents: mutable.WrappedArray[Int])
                               (implicit R: Ring[T]) {
  val arity = exponents.length
  val degree = exponents.sum
  def *(y: Monomial[T]) = {
    require(arity == y.arity)
    Monomial(R.*(coefficient, y.coefficient), (exponents, y.exponents).zipped map (_+_))
  }
  def unary_- = Monomial[T](R.unary_-(coefficient), exponents)
  def /?(y: Monomial[T]): Option[Monomial[T]] = {
    require(arity == y.arity)
    val qx = (exponents, y.exponents).zipped map (_-_)
    if (qx.forall(_ >= 0)) { R./?(coefficient, y.coefficient) match {
      case Some(quotient) => Some(Monomial(quotient, qx))
      case None => None
    }} else None
  }

  override def toString = "" + coefficient + "×" + exponents.mkString("⋅")
}

object Monomial {
  def make[T](coefficient: T, exponents: Seq[Int]) (implicit R: Ring[T]): Monomial[T] =
    Monomial(coefficient, exponents.toArray)
  object Ordering {
    object Lex extends Ordering[Monomial[_]] {
      override def compare(x: Monomial[_], y: Monomial[_]) = {
        val diff = (y.exponents, x.exponents).zipped map (_-_) dropWhile (_ == 0)
        if (diff.isEmpty) 0 else diff.head
      }
    }
    object GrLex extends Ordering[Monomial[_]] {
      override def compare(x: Monomial[_], y: Monomial[_]): Int = {
        val grade = y.degree - x.degree
        if (grade != 0) grade else Lex.compare(x, y)
      }
    }
  }
}

