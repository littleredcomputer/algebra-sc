package net.littleredcomputer.algebra

/**
  * Created by colin on 7/7/2016.
  */

case class Term[R] (coefficient: R, monomial: Monomial) {
  /**
    * Maps the coefficient from the ring R to the ring S. The exponents
    * are not modified.
    *
    * @param f the mapping
    * @tparam S the target ring
    * @return the term in the ring S under f
    */
  def map[S](f: R => S): Term[S] = Term(f(coefficient), monomial)

  /**
    * Maps the exponent sequence. The coefficient is unmodified and the base
    * ring stays the same.
    * @param f mapping of exponents
    * @return the new term
    */
  def mapExponents(f: Seq[Int] => Seq[Int]) = Term(coefficient, monomial map f)

  override def toString = coefficient + "×" + monomial
}
