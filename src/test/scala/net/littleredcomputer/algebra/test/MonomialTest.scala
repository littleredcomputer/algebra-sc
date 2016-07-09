package net.littleredcomputer.algebra.test

import net.littleredcomputer.algebra.Monomial
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by colin on 7/9/2016.
  */

class MonomialTest extends FlatSpec with Matchers {
  val x3 = Monomial(List(3, 0, 0))
  val z2 = Monomial(List(0, 0, 2))
  val x3y = Monomial(List(3, 1, 0))
  val x3z = Monomial(List(3, 0, 1))
  val x3z2 = Monomial(List(3, 0, 2))
  val x2y2z = Monomial(List(2, 2, 1))
  val x2yz2 = Monomial(List(2, 1, 2))
  val x2z2 = Monomial(List(2, 0, 2))
  val x2z = Monomial(List(2, 0, 1))
  val x2 = Monomial(List(2, 0, 0))
  val xy2z = Monomial(List(1, 2, 1))

  "Lex order" should "work" in {
    val f = Monomial.Ordering.Lex.compare _
    f(x3, x2z2) should be < 0
    f(x2z2, xy2z) should be < 0
    f(xy2z, z2) should be < 0
  }
  "GrLex order" should "work" in {
    val f = Monomial.Ordering.GrLex.compare _
    f(x2z2, xy2z) should be < 0
    f(xy2z, x3) should be < 0
    f(x3, z2) should be < 0
  }
  "Monomials" should "can be multiplied" in {
    Monomial(List(3, 1, 2)) * Monomial(List(1, 2, 3)) should be (Monomial(List(4, 3, 5)))
  }
  it should "can be exponentiated" in {
    Monomial(List(3, 1, 2)) ^ 3 should be (Monomial(List(9, 3, 6)))
  }
  it should "be mappable" in {
    Monomial(List(3,1,2)) map (_.tail) should be (Monomial(List(1,2)))
  }
}


