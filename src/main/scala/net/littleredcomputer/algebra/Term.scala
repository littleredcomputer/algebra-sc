package net.littleredcomputer.algebra


/**
  * Created by colin on 7/7/2016.
  */

case class Term[R] (coefficient: R, monomial: Monomial) (implicit R: Ring[R]) {
  /**
    * Maps the coefficient from the ring R to the ring S. The exponents
    * are not modified.
    * @param f the mapping
    * @param S the target ring
    * @tparam S the target ring
    * @return the term transported by f into the ring S
    */
  def map[S](f: R => S) (implicit S: Ring[S]) = Term(f(coefficient), monomial)

  /**
    * Maps the exponent sequence. The coefficient is unmodified and the base
    * ring stays the same.
    * @param f mapping of exponents
    * @return the new term
    */
  def mapx(f: Seq[Int] => Seq[Int]) = Term(coefficient, monomial map f)
  def *(y: Term[R]) = {
    require(monomial.arity == y.monomial.arity)
    Term(R.*(coefficient, y.coefficient), monomial * y.monomial)
  }
  def *(y: R) = Term(R.*(y, coefficient), monomial)
  def ^(y: Int) = Term(R.^(coefficient, y), monomial ^ y)
  def +(y: Term[R]) = Polynomial.make[R](List(this, y))
  def +(y: R) = Polynomial.make(List(Term(y, Monomial(List.fill(monomial.arity)(0))), this))
  def -(y: Term[R]) = Polynomial.make[R](List(this, -y))
  def -(y: R) = this + R.unary_-(y)
  def +(y: Polynomial[R]) = Polynomial.make(this :: y.terms)
  def unary_- = map(R.unary_-)

  /**
    * If y divides this (this will be true if the exponents of y are all lower
    * or equal to the corresponding exponents of this, and the coefficients
    * are divisible in the ring R), returns Some quotient of the terms, else
    * None.
    *
    * @param y the divisor
    * @return optionally, the quotient
    */
  def /?(y: Term[R]): Option[Term[R]] = {
    require(monomial.arity == y.monomial.arity)
    val qx = (monomial.exponents, y.monomial.exponents).zipped map (_-_)
    if (qx.forall(_ >= 0)) { R./?(coefficient, y.coefficient) match {
      case Some(quotient) => Some(Term[R](quotient, Monomial(qx)))
      case None => None
    }} else None
  }
  override def toString = coefficient + "×" + monomial
}

object Term {
  def variables[R](arity: Int) (implicit R: Ring[R]) = for {i <- 0 until arity} yield Term(R.one, Monomial.basis(i, arity))
}
