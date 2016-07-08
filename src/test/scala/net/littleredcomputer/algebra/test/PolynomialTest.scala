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

class MonomialOrderTest extends FlatSpec with Matchers {
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
  "Simple divisions" should "work" in {
    val x2m1 = Polynomial(List(Term(1, x*x), -one))
    val xp1 = Polynomial(List(Term(1, x), one))
    val xm1 = Polynomial(List(Term(1, x), -one))
    x2m1 divide xp1 should be (xm1, z)
    x2m1 divide xm1 should be (xp1, z)
  }
}

class PolynomialSuite extends FlatSpec with Matchers {
  val one = Polynomial(List(Term(1, Monomial(List(0,0)))))
  val x = Polynomial(List(Term(1, Monomial(List(1,0)))))
  val y = Polynomial(List(Term(1, Monomial(List(0,1)))))
  "Multiple quotient divisions" should "pass Ex.1 (p.61)" in {
    // CLO p.61
    val f = x*y*y + one
    val f1 = x*y + one
    val f2 = y + one
    f divide List(f1, f2) should be (List(y, -one), one * 2)
  }
  it should "pass Ex.2 (p.62)" in {
    val f = x*x*y + x*y*y + y*y
    val f1 = x*y - one
    val f2 = y*y - one
    f divide List(f1, f2) should  be (List(x + y, one), x + y + one)
  }
  it should "pass Ex.4 (p.66)" in {
    val f = x*x*y + x*y*y + y*y
    val f1 = y*y - one
    val f2 = x*y - one
    f divide List(f1, f2) should be (List(x + one, x), x + x + one)
  }
  it should "pass Ex.5 (p.67)" in {
    val f = x*y*y - x
    val f1 = x*y + one
    val f2 = y*y - one
    val zero = Polynomial.zero[Int]
    f divide List(f1, f2) should be (List(y, zero), - x - y)
    f divide List(f2, f1) should be (List(x, zero), zero)
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
