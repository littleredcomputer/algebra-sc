/**
  * Created by colin on 6/28/16.
  */
package net.littleredcomputer.algebra

import org.apache.commons.math3.fraction.BigFraction

trait Ring[T] {
  def zero: T
  def one: T
  def *(x: T, y: T): T
  def +(x: T, y: T): T
  def unary_-(x: T): T
  def /?(x: T, y: T): Option[T]
  def ^(x: T, y: Int): T
}

object Ring {
  implicit object Z extends Ring[Int] {
    def zero = 0
    def one = 1
    def *(x: Int, y: Int) = x * y
    def +(x: Int, y: Int) = x + y
    def unary_-(x: Int) = -x
    def /?(x: Int, y: Int) = {
      require(y != 0)
      if (x % y == 0) Some(x/y) else None
    }
    def ^(x: Int, y: Int) = (1 /: (1 to y)) { (v, _) => *(v, x) }  // slow. But the Ints are going to overflow anyway.
  }
  implicit object BigZ extends Ring[BigInt] {
    def zero = 0
    def one = 1
    def *(x: BigInt, y: BigInt) = x * y
    def +(x: BigInt, y: BigInt) = x + y
    def unary_-(x: BigInt) = -x
    def /?(x: BigInt, y: BigInt) = {
      require(y != 0)
      if (x % y == 0) Some(x/y) else None
    }
    def ^(x: BigInt, y: Int) = x.pow(y)
  }
  implicit object R extends Ring[Double] {
    def zero = 0.0
    def one = 1.0
    def *(x: Double, y: Double) = x * y
    def +(x: Double, y: Double) = x + y
    def unary_-(x: Double) = -x
    def /?(x: Double, y: Double) = {
      require(y != 0)
      Some(x/y)
    }
    def ^(x: Double, y: Int) = Math.pow(x, y)
  }
  implicit object Zx extends Ring[Polynomial[Int]] {
    def zero = Polynomial.make[Int](List())
    def one = ???
    def *(x: Polynomial[Int], y: Polynomial[Int]) = x * y
    def +(x: Polynomial[Int], y: Polynomial[Int]) = x + y
    def unary_-(x: Polynomial[Int]) = -x
    def /?(x: Polynomial[Int], y: Polynomial[Int]) = ???
    def ^(x: Polynomial[Int], y: Int) = ???
  }
  implicit object Q extends Ring[BigFraction] {
    def zero = BigFraction.ZERO
    def one = BigFraction.ONE
    def *(x: BigFraction, y: BigFraction) = x.multiply(y)
    def +(x: BigFraction, y: BigFraction) = x.add(y)
    def unary_-(x: BigFraction) = x.negate()
    def /?(x: BigFraction, y:BigFraction) = {
      require(y != BigFraction.ZERO)
      Some(x.divide(y))
    }
    def ^(x: BigFraction, y: Int) = x.pow(y)
  }
}
