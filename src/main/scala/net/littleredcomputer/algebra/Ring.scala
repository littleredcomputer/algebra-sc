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
}

object Ring {

  implicit object Z extends Ring[Int] {
    def zero = 0
    def *(x: Int, y: Int) = x * y
    def +(x: Int, y: Int) = x + y
    def unary_-(x: Int) = -x
  }
  implicit object R extends Ring[Double] {
    def zero = 0.0
    def *(x: Double, y: Double) = x * y
    def +(x: Double, y: Double) = x + y
    def unary_-(x: Double) = -x
  }
  implicit object Zx extends Ring[Polynomial[Int]] {
    def zero = Polynomial.make[Int](List())
    def *(x: Polynomial[Int], y: Polynomial[Int]) = x * y
    def +(x: Polynomial[Int], y: Polynomial[Int]) = x + y
    def unary_-(x: Polynomial[Int]) = -x
  }
  implicit object Q extends Ring[BigFraction] {
    def zero = BigFraction.ZERO
    def *(x: BigFraction, y: BigFraction) = x.multiply(y)
    def +(x: BigFraction, y: BigFraction) = x.add(y)
    def unary_-(x: BigFraction) = x.negate()
  }
}
