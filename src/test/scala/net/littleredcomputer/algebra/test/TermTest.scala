package net.littleredcomputer.algebra.test

import net.littleredcomputer.algebra.{Monomial, Polynomial, Term}
import org.scalatest._

/**
 * Created by colin on 7/9/2016.
 */

class TermTest extends FlatSpec with Matchers {
  val x = Term(1, Monomial(List(1, 0, 0)))
  val twox = Term(2, Monomial(List(1, 0, 0)))
  val y = Term(1, Monomial(List(0, 1, 0)))
  val z = Term(1, Monomial(List(0, 0, 1)))

  "Terms" should "be liftable to new rings with map" in {
    x map {
      _ / 2.0
    } should be(Term[Double](0.5, Monomial(List(1, 0, 0))))
  }
  it should "allow modification of exponents with mapx" in {
    x mapx (_.tail) should be(Term(1, Monomial(List(0, 0))))
    y mapx (_.tail) should be(Term(1, Monomial(List(1, 0))))
    z mapx (_.tail) should be(Term(1, Monomial(List(0, 1))))
  }
}
