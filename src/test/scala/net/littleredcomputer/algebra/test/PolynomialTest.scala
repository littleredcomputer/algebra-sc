package net.littleredcomputer.algebra.test

import net.littleredcomputer.algebra.{Monomial, Polynomial, Ring, Term}
import org.apache.commons.math3.fraction.BigFraction
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest._
import org.scalacheck.Gen._
import org.scalacheck.Test.Parameters

object Implicits {
  implicit val arbitraryRational: Arbitrary[BigFraction] = Arbitrary { for {
    n <- Gen.chooseNum(-5000,5000)
    d <- Gen.chooseNum(1,5000)
  } yield new BigFraction(n, d) }

  implicit def arbitraryPolynomial[T](implicit a: Arbitrary[T],
                                      R: Ring[T],
                                      arity: Int): Arbitrary[Polynomial[T]] = Arbitrary {
    def sizedPoly(sz: Int) = for {
      exponents <- Gen.listOfN(sz, Gen.listOfN(arity, choose(0, 6)))
      coefficients <- Gen.listOfN(sz, Arbitrary.arbitrary[T])
    } yield Polynomial.make((coefficients, exponents).zipped map {
      case (c, xs) => Term(c, Monomial(xs))
    })

    Gen.sized(sizedPoly)
  }
}

class MonomialSuite extends FlatSpec with Matchers {
  val x = Monomial(List(1))
  "Monomial multiplication" should "be commutative" in {
    val y = Monomial(List(2))
    x * y should be (y * x)
  }
  val one = Term(1, Monomial(List(0)))
  val z = Polynomial.zero[Int]

  "The zero polynomial" should "annihilate any other" in {
    val p = Polynomial(List(Term(1, x)))
    p * z should be (z)
    z * p should be (z)
  }
}

class UnivariateSuite extends FlatSpec with Matchers {
  val x = Monomial(List(1))
  val one = Term(1, Monomial(List(0)))
  val z = Polynomial.zero[Int]
  val x2m1 = Polynomial.make(List(Term(1, x*x), -one))
  val xp1 = Polynomial.make(List(Term(1, x), one))
  val xm1 = Polynomial.make(List(Term(1, x), -one))
  "Simple divisions" should "work" in {
    x2m1 divide xp1 should be (xm1, z)
    x2m1 divide xm1 should be (xp1, z)
  }
  "Evaluation" should "work" in {
    (xp1 * xm1).evaluate(5) should be (24)
  }
}

class RemainderTest extends FlatSpec with Matchers {
  def P(as: Int*) = Polynomial.makeDenseUnivariate[Int](as.toList)
  val u = P(-5, 2, 8, -3, -3, 0, 1, 0, 1)
  val v = P(21, -9, -4, 0, 5, 0, 3)
  val z = P()
  val uq = P(-5, 2, 8, -3, -3, 0, 1, 0, 1) map (new BigFraction(_))
  val vq = P(21, -9, -4, 0, 5, 0, 3) map (new BigFraction(_))
  val zq = Polynomial.makeDenseUnivariate[BigFraction](List())
  "Z[x]" should "be liftable to Q[x] via map" in {
    u map {new BigFraction(_)} should be (uq)
  }
  "Knuth's Example" should "work over Z" in {
    u divide v should be (z, u)
  }
  it should "work over Q" in {
    uq divide vq should be (
      Polynomial.makeDenseUnivariate[BigFraction](List(new BigFraction(-2, 9), BigFraction.ZERO, new BigFraction(1, 3))),
      Polynomial.makeDenseUnivariate[BigFraction](List(new BigFraction(-1, 3), BigFraction.ZERO, new BigFraction(1, 9), BigFraction.ZERO, new BigFraction(-5, 9)))
    )
  }
  it should "pseudo-divide over Z" in {
    u pseudoRemainder v should be (P(-3, 0, 1, 0, -5), 2)
  }
  "other pseudo-remainder examples" should "work" in {
    P(1, 1, 0, 1) pseudoRemainder P(1, 1, 3) should be (P(10, 7), 2)
    P(3, 0, 4) pseudoRemainder P(2, 2) should be (P(28), 2)
    P(7) pseudoRemainder P(2) should be (P(0), 1)
  }
}

abstract class VariablesTest[R] (implicit R: Ring[R]) extends FlatSpec with Matchers {
  Term.variables[R](3) match {
    case Seq(x, y, z) =>
      "variables" should "produce usable terms" in {
        x should be (Term[R](R.one, Monomial(List(1, 0, 0))))
        y should be (Term[R](R.one, Monomial(List(0, 1, 0))))
        x*y should be (Term[R](R.one, Monomial(List(1, 1, 0))))
      }
      it should "produce polynomials when added" in {
        x+y should be (Polynomial.make[R](List(x, y)))
      }
      it should "terms can exponentiate" in {
        z^5 should be(Term[R](R.one, Monomial(List(0, 0, 5))))
      }
  }
}

class ZVariablesTest extends VariablesTest[Int] {
  Term.variables[Int](3) match {
    case Seq(x, y, z) =>
      it should "accept addition by a constant" in {
        y+5 should be (Polynomial.make(List(y, Term(5, Monomial(List(0,0,0))))))
        y+(z+5) should be (Polynomial.make(z :: (y+5).terms))
        (y+z)+5 should be (Polynomial.make(z :: (y+5).terms))
        y+z+5 should be (Polynomial.make(z :: (y+5).terms))
      }
  }
}

class QVariablesTest extends VariablesTest[BigFraction] {}
class RVariablesTest extends VariablesTest[Double] {}
class BVariablesTest extends VariablesTest[BigInt] {}

class PolynomialSuite extends FlatSpec with Matchers {
  val one = Polynomial.make(List(Term(1, Monomial(List(0, 0)))))
  println("one", one)
  println("one*2", one * 2)
  Term.variables[Int](2) match {
    case Seq(x, y) =>
      "Multiple quotient divisions" should "pass Ex.1 (p.61)" in {
        // CLO p.61
        val f = x*y*y + 1
        val f1 = x*y + 1
        val f2 = y + 1
        f divide List(f1, f2) should be (List(Polynomial(List(y)), -one), one * 2)
      }
      it should "pass Ex.2 (p.62)" in {
        val f = x*x*y + x*y*y + y*y
        val f1 = x*y - 1
        val f2 = y*y - 1
        f divide List(f1, f2) should be (List(x + y, one), x + y + 1)
      }
      it should "pass Ex.4 (p.66)" in {
        val f = x*x*y + x*y*y + y*y
        val f1 = y*y - 1
        val f2 = x*y - 1
        f divide List(f1, f2) should be (List(x + 1, Polynomial(List(x))), x + x + 1)
      }
      it should "pass Ex.5 (p.67)" in {
        val f = x*y*y - x
        val f1 = x*y + 1
        val f2 = y*y - 1
        val zero = Polynomial.zero[Int]
        f divide List(f1, f2) should be (List(Polynomial(List(y)), zero), - x - y)
        f divide List(f2, f1) should be (List(Polynomial(List(x)), zero), zero)
      }
      "Lowering arity" should "work" in {
        val p = y*(x^2) + (x^2) + x*y - x + y
        val q = p.lower
        val w = Term[Int](1, Monomial(List(1)))
        val o = Term[Int](1, Monomial(List(0)))
        q should be (Polynomial(List(Term(w + o, Monomial(List(2))), Term(w - o, Monomial(List(1))), Term(Polynomial(List(w)), Monomial(List(0))))))
      }
  }
}

// Tricky to figure out how to supply the necessary implicits to this style of test!
//class PTestZ2 extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
//  implicit val arity = 2
//  import Implicits.arbitraryPolynomial
//  val z = Polynomial.zero[Int]
//  type Zx = Polynomial[Int]
//  forAll { (p: Polynomial[Int], q: Polynomial[Int]) =>
//    p + q should be (q + p)
//  } (generatorDrivenConfig, Implicits.arbitraryPolynomial[Int], null, Implicits.arbitraryPolynomial[Int], null)
////  forAll (Implicits.arbitraryPolynomial[Int], Implicits.arbitraryPolynomial[Int]) {
////    (p: Polynomial[Int], q: Polynomial[Int]) =>
////      p + q should be (q + p)
////  }
//
//}

object PTestZ extends Properties("Polynomial[Int]") {
  import Implicits.arbitraryPolynomial
  type Zx = Polynomial[Int]
  val z = Polynomial.zero[Int]
  for (x <- 1 to 3) {
    implicit val arity = x
    property("+ is commutative a=" + arity) = forAll {
      (p: Zx, q: Zx) => p + q == q + p
    }
    property("* is comm. a=" + arity) = forAll {
      (p: Zx, q: Zx) => p * q == q * p
    }
    property("* distributes over + a=" + arity) = forAll {
      (p: Zx, q: Zx, r: Zx) => p * (q + r) == p * q + p * r
    }
    property("p - p == 0, a=" + arity) = forAll {
      (p: Zx) => p - p == z
    }
    property("0p = 0, a=" + arity) = forAll {
      (p: Zx) => p * z == z
    }
  }
}

object DivTestZ extends Properties("DivTest[BigZ]") {
  import Implicits.arbitraryPolynomial
  for (x <- 1 to 3) {
    implicit val arity = x
    type BZx = Polynomial[BigInt]
    property("pq / p == q, a=" + arity) = forAll {
      (p: BZx, q: BZx) => (!p.isZero && !q.isZero) ==> (((p * q) divide p) == (q, Polynomial.zero[BigInt]))
    }
  }
}

object PTestQ extends Properties("Polynomial[BigFraction]") {
  import Implicits.arbitraryRational
  import Implicits.arbitraryPolynomial
  override def overrideParameters(p: Parameters): Parameters = p.withMinSuccessfulTests(25)
  type Qx = Polynomial[BigFraction]

  for (x <- 1 to 3) {
    implicit val arity = x
    property("+ is commutative a=" + arity) = forAll {
      (p: Polynomial[BigFraction], q: Polynomial[BigFraction]) => p + q == q + p
    }
    property("* is comm. a=" + arity) = forAll {
      (p: Qx, q: Qx) => p * q == q * p
    }
    property("* distributes over + a=" + arity) = forAll {
      (p: Qx, q: Qx, r: Qx) => p * (q + r) == p * q + p * r
    }
  }
}
