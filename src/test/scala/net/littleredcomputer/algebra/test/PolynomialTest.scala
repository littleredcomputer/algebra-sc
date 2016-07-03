package net.littleredcomputer.algebra.test

import net.littleredcomputer.algebra.{Monomial, Polynomial}
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalatest._
import org.scalatest.prop.PropertyChecks

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

// this almost compiles, but we need to provide a generator for polynomials.
//object PolySpec extends Properties("Polynomial[Int]") {
//  property("commutative") = forAll { (p: Polynomial[Int], q: Polynomial[Int]) =>
//    (p * q) == (q * p)
//  }
//}

//class FooSpec extends WordSpec with PropertyChecks {
//  "polynomail multplication" must {
//    "be commutative" in {
//      forAll { (p: Polynomial[Int], q: Polynomial[Int]) =>
//        p * q should be (q * p)
//
//      }
//    }
//  }
//}
/**
  * Created by colin on 7/3/2016.
  */
class PolynomialTest {

}
