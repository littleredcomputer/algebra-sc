package net.littleredcomputer.algebra.test

import net.littleredcomputer.algebra._
import org.apache.commons.math3.fraction.BigFraction
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object Implicits {
  implicit val arbitraryRational: Arbitrary[BigFraction] = Arbitrary { for {
    n <- Gen.chooseNum(-5000,5000)
    d <- Gen.chooseNum(1,5000)
  } yield new BigFraction(n, d) }

  implicit def arbitraryPolynomial[T](implicit a: Arbitrary[T],
                                      R: EuclideanRing[T],
                                      O: Ordering[Monomial],
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

class UnivariateSuite extends FlatSpec with Matchers {
  implicit def o: Ordering[Monomial] = Monomial.Ordering.GrLex
  val x = Monomial(List(1))
  val one = Term(1, Monomial(List(0)))
  val mone = Term(-1, Monomial(List(0)))
  val z = Polynomial.zero[Int]
  val x2m1 = Polynomial.make(List(Term(1, x*x), mone))
  val xp1 = Polynomial.make(List(Term(1, x), one))
  val xm1 = Polynomial.make(List(Term(1, x), mone))
  "Simple divisions" should "work" in {
    x2m1 divide xp1 should be (xm1, z)
    x2m1 divide xm1 should be (xp1, z)
  }
  "Evaluation" should "work" in {
    (xp1 * xm1).evaluate(5) should be (24)
  }
}

class RemainderTest extends FlatSpec with Matchers {
  implicit def o: Ordering[Monomial] = Monomial.Ordering.GrLex
  import scala.language.implicitConversions
  implicit def toRational(x: Int): BigFraction = new BigFraction(x)
  def P(as: Int*) = Polynomial.makeDenseUnivariate[Int](as.toList)
  val u = P(-5, 2, 8, -3, -3, 0, 1, 0, 1)
  val v = P(21, -9, -4, 0, 5, 0, 3)
  val z = P()
  val uq = P(-5, 2, 8, -3, -3, 0, 1, 0, 1) map (new BigFraction(_))
  val vq = P(21, -9, -4, 0, 5, 0, 3) map (new BigFraction(_))
  val zq = Polynomial.makeDenseUnivariate[BigFraction](List())
  "Z[x]" should "lift to Q[x] via map" in {
    u map {new BigFraction(_)} should be (uq)
  }
  "Knuth's Example" should "work over Z" in {
    u divide v should be (z, u)
  }
  it should "work over Q" in {
    uq divide vq should be (
      Polynomial.makeDenseUnivariate[BigFraction](List(-2 divide 9, 0, 1 divide 3)),
      Polynomial.makeDenseUnivariate[BigFraction](List(-1 divide 3, 0, 1 divide 9, 0, -5 divide 9))
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

abstract class VariablesTest[R] (implicit R: EuclideanRing[R]) extends FlatSpec with Matchers {
  implicit def o: Ordering[Monomial] = Monomial.Ordering.GrLex
  Polynomial.vars3[R] { (x, y, z) =>
    "variables" should "produce usable one-term polynomials" in {
      x should be (Polynomial.make[R](List(Term(R.one, Monomial(List(1, 0, 0))))))
      y should be (Polynomial.make[R](List(Term(R.one, Monomial(List(0, 1, 0))))))
      x*y should be (Polynomial.make[R](List(Term(R.one, Monomial(List(1, 1, 0))))))
    }
    it should "produce polynomials when added" in {
      x+y should be (Polynomial.make[R](List(x.leadingTerm, y.leadingTerm)))
    }
    it should "terms can exponentiate" in {
      z^5 should be(Polynomial.make[R](List(Term[R](R.one, Monomial(List(0, 0, 5))))))
    }
  }
}

class ZVariablesTest extends VariablesTest[Int] {
  Polynomial.vars3[Int] { (x, y, z) =>
    it should "accept addition by a constant" in {
      y+5 should be (Polynomial.make(List(y.leadingTerm, Term(5, Monomial(List(0,0,0))))))
      y+(z+5) should be (Polynomial.make(z.leadingTerm :: (y+5).terms))
      (y+z)+5 should be (Polynomial.make(z.leadingTerm :: (y+5).terms))
      y+z+5 should be (Polynomial.make(z.leadingTerm :: (y+5).terms))
    }
  }
}

class QVariablesTest extends VariablesTest[BigFraction] {}
class RVariablesTest extends VariablesTest[Double] {}
class BVariablesTest extends VariablesTest[BigInt] {}

class PolynomialSuite extends FlatSpec with Matchers {
  implicit def o: Ordering[Monomial] = Monomial.Ordering.GrLex
  val one = Polynomial.make(List(Term(1, Monomial(List(0, 0)))))
  Polynomial.vars2[Int] { (x, y) =>
    "Multiple quotient divisions" should "pass Ex.1 (p.61)" in {
      // CLO p.61
      val f = x * y * y + 1
      val f1 = x * y + 1
      val f2 = y + 1
      f divide List(f1, f2) should be(List(y, -one), one * 2)
    }
    it should "pass Ex.2 (p.62)" in {
      val f = x * x * y + x * y * y + y * y
      val f1 = x * y - 1
      val f2 = y * y - 1
      f divide List(f1, f2) should be(List(x + y, one), x + y + 1)
    }
    it should "pass Ex.4 (p.66)" in {
      val f = x * x * y + x * y * y + y * y
      val f1 = y * y - 1
      val f2 = x * y - 1
      f divide List(f1, f2) should be(List(x + 1, x), x + x + 1)
    }
    it should "pass Ex.5 (p.67)" in {
      val f = x * y * y - x
      val f1 = x * y + 1
      val f2 = y * y - 1
      val zero = Polynomial.zero[Int]
      f divide List(f1, f2) should be(List(y, zero), -x - y)
      f divide List(f2, f1) should be(List(x, zero), zero)
    }
    "Lowering arity" should "work" in {
      val p = y * (x ^ 2) + (x ^ 2) + x * y - x + y
      val q = p.lower
      Polynomial.vars1[Int] { w =>
        q should be(Polynomial(List(Term(w + 1, Monomial(List(2))), Term(w - 1, Monomial(List(1))), Term(w, Monomial(List(0))))))
      }
    }
  }
}

class SPolynomialTest extends FlatSpec with Matchers {
  implicit def defaultOrder: Ordering[Monomial] = Monomial.Ordering.GrLex
  Polynomial.vars2[BigFraction] { (x, y) =>
    val f = (x ^ 3) * (y ^ 2) - (x ^ 2) * (y ^ 3) + x
    val g = (x ^ 4) * y * new BigFraction(3) + (y ^ 2)
    "S polynomials" should "check over Q" in {
      f S g should be(- (x^3) * (y^3) + (x^2) - (y^3) * BigFraction.ONE_THIRD)
    }
  }
  Polynomial.vars3[BigFraction] { (y, z, x) =>
    val f = y - (x^2)
    val g = z - (x^3)

    it should "check over Q (p.87)" in {
      f S g should be(-z * (x^2) + y * (x^3))
    }
  } (EuclideanRing.Q, Monomial.Ordering.Lex)
}

class GroebnerBasisTest extends FlatSpec with Matchers {
  "Example 2.7.1" should "work" in {
    implicit def o = Monomial.Ordering.GrLex
    Polynomial.vars2[BigFraction] { (x, y) =>
      val f1 = (x ^ 3) - x * y * BigFraction.TWO
      val f2 = (x ^ 2) * y - (y ^ 2) * BigFraction.TWO + x
      GroebnerBasis.of(f1, f2) should be(Set(
        (x ^ 3) - x * y * BigFraction.TWO,
        (x ^ 2) * y - (y ^ 2) * BigFraction.TWO + x,
        -(x ^ 2),
        -x * y * BigFraction.TWO,
        -(y ^ 2) * BigFraction.TWO + x
      ))
    }
  }
  // not yet!
  "Example 2.8.2" should "work" in {
    Polynomial.vars3[BigFraction] { (x, y, z) =>
      val f1 = (x^2) + (y^2) + (z^2) - BigFraction.ONE
      val f2 = (x^2) + (z^2) - y
      val f3 = x - z
      GroebnerBasis.of(f1, f2, f3) should contain allOf(f3, -y+(z^2)*BigFraction.TWO, (z^4)*(new BigFraction(4)) + (z^2)*BigFraction.TWO - BigFraction.ONE)
    } (EuclideanRing.Q, Monomial.Ordering.Lex)
  }
}

object PTestZ extends Properties("Polynomial[Int]") {
  implicit def defaultOrder: Ordering[Monomial] = Monomial.Ordering.GrLex
  import Implicits.arbitraryPolynomial
  type Zx = Polynomial[Int]
  val z = Polynomial.zero[Int]
  for (x <- 1 to 3) {
    implicit val arity = x
    property("+ is commutative a=" + arity) = forAll {
      (p: Zx, q: Zx) => p + q == q + p
    }
    property("* is commutative a=" + arity) = forAll {
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

class PTestBigZ extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  import Implicits.arbitraryPolynomial
  implicit def defaultOrder: Ordering[Monomial] = Monomial.Ordering.GrLex

  override implicit val generatorDrivenConfig: PropertyCheckConfig = PropertyCheckConfig(minSuccessful = 20)
  type BZx = Polynomial[BigInt]
  for (a <- 1 to 3) {
    implicit val arity = a
    "polynomials over BigZ" should "divide correctly, arity " + arity in {
      forAll {
        (p: BZx, q: BZx) => whenever(!p.isZero && !q.isZero) {
          (p * q) divide p should be ((q, Polynomial.zero[BigInt]))
        }
      }
    }
  }
}

class PTestQ extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit def o: Ordering[Monomial] = Monomial.Ordering.GrLex
  import Implicits.{arbitraryPolynomial, arbitraryRational}
  override implicit val generatorDrivenConfig: PropertyCheckConfig = PropertyCheckConfig(minSuccessful = 20)
  type Qx = Polynomial[BigFraction]
  for (x <- 1 to 3) {
    implicit val arity = x
    "polynomials over Q" should "be commutative over addition, arity " + arity in {
      forAll {
        (p: Qx, q: Qx) => p + q should be (q + p)
      }
    }
    it should "be commutative over multiplication, arity " + arity in {
      forAll {
        (p: Qx, q: Qx) => p * q should be (q * p)
      }
    }
    it should "satisfy distributivity of * over +, arity " + arity in {
      forAll {
        (p: Qx, q: Qx, r: Qx) => p * (q + r) should be (p * q + p * r)
      }
    }
  }
}

