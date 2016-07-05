package net.littleredcomputer.algebra.test

import net.littleredcomputer.algebra.{Monomial, Polynomial, Ring}
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import org.scalacheck.Prop.forAll
import org.scalatest._
import org.scalacheck.Gen._
import org.scalatest.prop.PropertyChecks

object Implicits {
  implicit def arbPolynomial[T] (implicit a: Arbitrary[T],
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


class PolynomialSuite extends FlatSpec with Matchers {

  val x = Monomial(1,Vector(1))
  "Monomial multiplication" should "be commutative" in {
    val y = Monomial(1,Vector(2))
    //val q = Polynomial.make(List(x))
    x * y should be (y * x)
  }

  "The zero polynomial" should "annihilate any other" in {
    val p = Polynomial(List(x))
    val z = Polynomial.zero[Int]
    p * z should be (z)
    z * p should be (z)
  }
}

object FooTest extends Properties("Foo") {
  property("+ is comm.") = forAll {
    (p: Int, q: Int) =>
      {
        println("p", p, "q", q)
        p+q == q+p
      }
  }
  val h = choose(1,5)
  property("look at sizes") = forAll(h) {
    n => { println ("n", n); n == n }
  }
}

object PTest extends Properties("Polynomial[Int]") {
  type Zx = Polynomial[Int]
  // this is promising. but we are setting arity to 1 because we have to fix monomial order.
  
  implicit val arity: Int = 1
  import Implicits.arbPolynomial

  property("+ is comm.") = forAll {
    (p: Zx, q: Zx) => p + q == q + p
  }

  property("* is comm.") = forAll {
    (p: Zx, q: Zx) => p * q == q * p
  }

  property("* distributes over +") = forAll {
    (p: Zx, q: Zx, r: Zx) => p * (q + r) == p * q + p * r
  }
}

