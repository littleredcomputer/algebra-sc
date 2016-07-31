package net.littleredcomputer.algebra.test

import net.littleredcomputer.algebra.{EuclideanRing, GroebnerBasis, Monomial, Polynomial}
import org.apache.commons.math3.fraction.BigFraction
import org.scalactic.Normalization
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by colin on 7/28/16.
  */
class GroebnerBasisTest extends FlatSpec with Matchers {
  "Example 2.7.1" should "work" in {
    implicit def o = Monomial.Ordering.GrLex
    Polynomial.vars2[BigFraction] { (x, y) =>
      val f1 = (x ^ 3) - x * y * BigFraction.TWO
      val f2 = (x ^ 2) * y - (y ^ 2) * BigFraction.TWO + x
      val B = Set(
        (x ^ 3) - x * y * BigFraction.TWO,
        (x ^ 2) * y - (y ^ 2) * BigFraction.TWO + x,
        -(x ^ 2),
        -x * y * BigFraction.TWO,
        -(y ^ 2) * BigFraction.TWO + x
      )
      //val z: Normalization[]
      GroebnerBasis.of(f1, f2) should be(B)
      (GroebnerBasis.Buchberger2(List(f1,f2)) map (_.abs)) should contain theSameElementsAs (B map (_.abs))
    }
  }
  "Example 2.8.2" should "work" in {
    Polynomial.vars3[BigFraction] { (x, y, z) =>
      val f1 = (x^2) + (y^2) + (z^2) - BigFraction.ONE
      val f2 = (x^2) + (z^2) - y
      val f3 = x - z
      val B = Set(f3,
        -y+(z^2) * BigFraction.TWO,
        (z^4) * new BigFraction(4) + (z^2) * BigFraction.TWO - BigFraction.ONE)

      // We catch the right basis elements, but also manage to net a bunch of extra ones.
      B.subsetOf(GroebnerBasis.of(f1, f2, f3)) should be(true)
      B.subsetOf(GroebnerBasis.Buchberger2(List(f1, f2, f3)).toSet) should be(true)
    } (EuclideanRing.Q, Monomial.Ordering.Lex)


  }
  "Example 2.8.4" should "work" in {
    implicit def o = Monomial.Ordering.Lex
    Polynomial.vars4[BigFraction] { (t, x, y, z) =>
      val f1 = (t^4) - x
      val f2 = (t^3) - y
      val f3 = (t^2) - z
      GroebnerBasis.of(f1, f2, f3) should contain allOf ((y^2) - (z^3), t*z - y /* and others XXX */)
      // We want this: (z - (t^2), t*y - (z^2), t*z - y, x - (z^2), (y^2) - (z^3))
      // but we don't quite get it (one of our basis elements is negated and one is
      // missing but is found as the sum of two others that we do produce. We need
      // to be chooser as to which elements we admit, or reduce after completion,
      // or both; and even that may not be sufficient since we don't yet understand
      // how these bases can be canonicalized.

      GroebnerBasis.Buchberger2(List(f1,f2,f3)) should contain allOf ((y^2) - (z^3), t*z - y /* and others XXX */)
    }
  }
}
