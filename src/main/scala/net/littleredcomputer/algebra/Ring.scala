/**
  * Created by colin on 6/28/16.
  */
package net.littleredcomputer.algebra

import org.apache.commons.math3.fraction.BigFraction

trait Ring[T] {
  def zero: T
  def *(x: T, y: T): T
  def +(x: T, y: T): T
  def unary_-(x: T): T
  def /?(x: T, y: T): Option[T]
  def expt(x: T, y: Int): T
}

object Ring {
  implicit object Z extends Ring[Int] {
    def zero = 0
    def *(x: Int, y: Int) = x * y
    def +(x: Int, y: Int) = x + y
    def unary_-(x: Int) = -x
    def /?(x: Int, y: Int) = {
      require(y != 0)
      if (x % y == 0) Some(x/y) else None
    }
    def expt(x: Int, y: Int) = (1 /: (1 to y)) { (v, _) => *(v, x) }  // slow. But the Ints are going to overflow anyway.

  }
  implicit object BigZ extends Ring[BigInt] {
    def zero = 0
    def *(x: BigInt, y: BigInt) = x * y
    def +(x: BigInt, y: BigInt) = x + y
    def unary_-(x: BigInt) = -x
    def /?(x: BigInt, y: BigInt) = {
      require(y != 0)
      if (x % y == 0) Some(x/y) else None
    }
    def expt(x: BigInt, y: Int) = ???
  }
  implicit object R extends Ring[Double] {
    def zero = 0.0
    def *(x: Double, y: Double) = x * y
    def +(x: Double, y: Double) = x + y
    def unary_-(x: Double) = -x
    def /?(x: Double, y: Double) = {
      require(y != 0)
      Some(x/y)
    }
    def expt(x: Double, y: Int) = Math.pow(x, y)
  }
  implicit object Zx extends Ring[Polynomial[Int]] {
    def zero = Polynomial.make[Int](List())
    def *(x: Polynomial[Int], y: Polynomial[Int]) = x * y
    def +(x: Polynomial[Int], y: Polynomial[Int]) = x + y
    def unary_-(x: Polynomial[Int]) = -x
    def /?(x: Polynomial[Int], y: Polynomial[Int]) = ???
    def expt(x: Polynomial[Int], y: Int) = ???
  }
  implicit object Q extends Ring[BigFraction] {
    def zero = BigFraction.ZERO
    def *(x: BigFraction, y: BigFraction) = x.multiply(y)
    def +(x: BigFraction, y: BigFraction) = x.add(y)
    def unary_-(x: BigFraction) = x.negate()
    def /?(x: BigFraction, y:BigFraction) = {
      require(y != BigFraction.ZERO)
      Some(x.divide(y))
    }
    def expt(x: BigFraction, y: Int) = x.pow(y)
  }
}
