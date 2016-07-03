package net.littleredcomputer.algebra.test

import net.littleredcomputer.algebra.{Monomial, Polynomial}
import org.scalatest._

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
/**
  * Created by colin on 7/3/2016.
  */
class PolynomialTest {

}
