package net.littleredcomputer.algebra.test

import net.littleredcomputer.algebra.{Monomial, Polynomial, Term}
import org.scalatest._

/**
 * Created by colin on 7/9/2016.
 */

class TermTest extends FlatSpec with Matchers {
  Term.variables[Int](3) match {
    case Seq(x, y, z) =>

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
      it should "allow multiplication by other terms" in {
        x * x should be(Term(1, Monomial(List(2, 0, 0))))
        twox * twox * twox should be(Term(8, Monomial(List(3, 0, 0))))
      }
      it should "allow multiplication by scalars" in {
        x * 7 should be(Term(7, x.monomial))
        twox * 3 should be(Term(6, x.monomial))
      }
      it should "allow exponentiation" in {
        x ^ 3 should be(Term(1, Monomial(List(3, 0, 0))))
        (twox * y * y) ^ 3 should be(Term(8, Monomial(List(3, 6, 0))))
      }
      it should "form polynomials from sums with other terms" in {
        x + y should be(Polynomial.make(List(x, y)))
        x + x should be(Polynomial.make(List(x, x)))
      }
      it should "form polyomials from sums with scalars" in {
        x + 3 should be(Polynomial.make(List(x, Term(3, Monomial(List(0, 0, 0))))))
        x - 3 should be(Polynomial.make(List(x, Term(-3, Monomial(List(0, 0, 0))))))
      }
      it should "allow subtraction" in {
        x - y should be(Polynomial.make(List(Term(1, Monomial(List(1, 0, 0))), Term(-1, Monomial(List(0, 1, 0))))))
        z - z should be(Polynomial.make[Int](List()))
      }
      it should "allow unary negation" in {
        -twox should be(Term(-2, Monomial(List(1, 0, 0))))
      }
      it should "sum with polynomials" in {
        x + (y + z) should be(Polynomial.make(List(x, y, z)))
      }
      it should "allow division" in {
        twox /? x should be(Some(Term(2, Monomial(List(0, 0, 0)))))
        (twox ^ 2) /? x should be(Some(twox * 2))
      }
      it should "disallow division when the quotient doesn't exist in the ring" in {
        x /? twox should be(None)
      }
  }
}
