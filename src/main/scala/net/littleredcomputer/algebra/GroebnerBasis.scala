package net.littleredcomputer.algebra

/**
  * Created by colin on 7/28/16.
  */
object GroebnerBasis {
  private def pairs[T](s: List[T]): List[(T, T)] = s match {
    case x :: xs => (for {y <- xs} yield (x, y)) ++ pairs(xs)
    case Nil => Nil
  }
  def Buchberger[F](fs: List[Polynomial[F]])(implicit F: Field[F]) = {
    var G = fs.toSet
    var Gprime = G.empty
    var done = false
    while (!done) {
      Gprime = G
      pairs(Gprime.toList) foreach { case ((p: Polynomial[F], q: Polynomial[F])) =>
        val (_, r) = (p S q) divide Gprime.toList
        if (!r.isZero) G += r
      }
      if (G == Gprime) done = true
    }
    G
  }
  def of[F](fs: Polynomial[F]*)(implicit F: Field[F]) = Buchberger(fs.toList)
}
