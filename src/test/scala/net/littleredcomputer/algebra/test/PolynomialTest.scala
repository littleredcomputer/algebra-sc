package net.littleredcomputer.algebra.test

import net.littleredcomputer.algebra.{Monomial, Polynomial, Ring}
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll
import org.scalatest._
import org.scalacheck.Gen._

object Implicits {
  implicit def arbitraryPolynomial[T](implicit a: Arbitrary[T],
                                      R: Ring[T],
                                      arity: Int): Arbitrary[Polynomial[T]] = Arbitrary {
    def sizedPoly(sz: Int) = for {
      exponents <- Gen.listOfN(sz, Gen.listOfN(arity, choose(0, 6)))
      coefficients <- Gen.listOfN(sz, Arbitrary.arbitrary[T])
    } yield Polynomial((coefficients, exponents).zipped map {
      case (c, xs) => Monomial(c, xs.toVector)
    })

    Gen.sized(sizedPoly)
  }
}

class MonomialOrderTest extends FlatSpec with Matchers {
  val x3 = Monomial(1, Vector(3, 0, 0))
  val z2 = Monomial(1, Vector(0, 0, 2))
  val x3y = Monomial(1, Vector(3, 1, 0))
  val x3z = Monomial(1, Vector(3, 0, 1))
  val x3z2 = Monomial(1, Vector(3, 0, 2))
  val x2y2z = Monomial(1, Vector(2, 2, 1))
  val x2yz2 = Monomial(1, Vector(2, 1, 2))
  val x2z2 = Monomial(1, Vector(2, 0, 2))
  val x2z = Monomial(1, Vector(2, 0, 1))
  val x2 = Monomial(1, Vector(2, 0, 0))
  val xy2z = Monomial(1, Vector(1, 2, 1))

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

class PolynomialSuite extends FlatSpec with Matchers {

  val x = Monomial(1,Vector(1))
  "Monomial multiplication" should "be commutative" in {
    val y = Monomial(1,Vector(2))
    x * y should be (y * x)
  }

  "The zero polynomial" should "annihilate any other" in {
    val p = Polynomial(List(x))
    val z = Polynomial.zero[Int]
    p * z should be (z)
    z * p should be (z)
  }
}

object PTest extends Properties("Polynomial[Int]") {
  import Implicits.arbitraryPolynomial
  type Zx = Polynomial[Int]
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
  }
}

