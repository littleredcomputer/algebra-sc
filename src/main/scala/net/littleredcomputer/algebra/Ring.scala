/**
  * Created by colin on 6/28/16.
  */
package net.littleredcomputer.algebra

import org.apache.commons.math3.fraction.BigFraction

import scala.annotation.tailrec

trait Ring[T] {
  def zero: T
  def one: T
  def *(x: T, y: T): T
  def +(x: T, y: T): T
  def unary_-(x: T): T
  def /%(x: T, y: T): (T, T)
  def ^(x: T, e: Int): T = {
    @tailrec def step(x: T, e: Int, r: T): T = if (e == 0) r
      else if (e % 2 == 0) step(*(x, x), e / 2, r)
      else step(x, e - 1, *(x, r))
    step(x, e, one)
  }
}

trait EuclideanRing[R] extends Ring[R] {
  def booya = 999
}

object Ring {
  implicit object Z extends Ring[Int] {
    override def zero = 0
    override def one = 1
    override def *(x: Int, y: Int) = x * y
    override def +(x: Int, y: Int) = x + y
    override def unary_-(x: Int) = -x
    override def /%(x: Int, y: Int) = (x/y, x%y)
  }
  implicit object BigZ extends Ring[BigInt] {
    override def zero = 0
    override def one = 1
    override def *(x: BigInt, y: BigInt) = x * y
    override def +(x: BigInt, y: BigInt) = x + y
    override def unary_-(x: BigInt) = -x
    override def /%(x: BigInt, y: BigInt) = x /% y
    override def ^(x: BigInt, y: Int) = x.pow(y)
  }
  implicit object R extends Ring[Double] {
    override def zero = 0.0
    override def one = 1.0
    override def *(x: Double, y: Double) = x * y
    override def +(x: Double, y: Double) = x + y
    override def unary_-(x: Double) = -x
    override def /%(x: Double, y: Double) = (x/y, 0.0)
    override def ^(x: Double, y: Int) = Math.pow(x, y)
  }
  implicit object Zx extends Ring[Polynomial[Int]] {
    override def zero = Polynomial.make[Int](List())
    override def one = ???
    override def *(x: Polynomial[Int], y: Polynomial[Int]) = x * y
    override def +(x: Polynomial[Int], y: Polynomial[Int]) = x + y
    override def unary_-(x: Polynomial[Int]) = -x
    override def /%(x: Polynomial[Int], y: Polynomial[Int]) = ???
  }
  implicit object Q extends Ring[BigFraction] {
    override def zero = BigFraction.ZERO
    override def one = BigFraction.ONE
    override def *(x: BigFraction, y: BigFraction) = x.multiply(y)
    override def +(x: BigFraction, y: BigFraction) = x.add(y)
    override def unary_-(x: BigFraction) = x.negate()
    override def /%(x: BigFraction, y: BigFraction) = (x divide y, BigFraction.ZERO)
    override def ^(x: BigFraction, y: Int) = x.pow(y)
  }
}
