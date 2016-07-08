package net.littleredcomputer.algebra

/**
  * Created by colin on 7/7/2016.
  */

case class Term[R] (coefficient: R, monomial: Monomial) (implicit R: Ring[R]) {
  def map[S](f: R => S) (implicit S: Ring[S]) = Term(f(coefficient), monomial)
  def mapm(f: Seq[Int] => Seq[Int]) = Term(coefficient, monomial map f)
  def *(y: Term[R]) = {
    require(monomial.arity == y.monomial.arity)
    Term(R.*(coefficient, y.coefficient), monomial * y.monomial)
  }
  def unary_- = map(R.unary_-)
  def /?(y: Term[R]): Option[Term[R]] = {
    require(monomial.arity == y.monomial.arity)
    val qx = (monomial.exponents, y.monomial.exponents).zipped map (_-_)
    if (qx.forall(_ >= 0)) { R./?(coefficient, y.coefficient) match {
      case Some(quotient) => Some(Term[R](quotient, Monomial(qx)))
      case None => None
    }} else None
  }
  override def toString = "" + coefficient + "×" + monomial
}

