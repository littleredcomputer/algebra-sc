package net.littleredcomputer.algebra

/**
  * Created by colin on 7/28/16.
  */
object GroebnerBasis {
  private def pairs[T](s: Seq[T]): List[(T, T)] = s match {
    case x :: xs => (for {y <- xs} yield (x, y)) ++ pairs(xs)
    case Nil => Nil
  }
  def Buchberger[F](fs: Seq[Polynomial[F]]) (implicit F: Field[F]) = {
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
  def kpairs(k: Int) = for {
    m <- 0 until k
    n <- m+1 until k
  } yield (m, n)
  def Buchberger2[F](fs: Seq[Polynomial[F]]) (implicit F: Field[F]) = {
    var B = kpairs(fs.length)
    var G = fs.toVector
    var t = fs.length - 1
    while (B.nonEmpty) {
      val (i, j) = B.head
      if (
        /*G(i).leadingTerm lcm G(j).leadingTerm != G(i).leadingTerm * G(j).leadingTerm*/
      true) {
        val s = (G(i) S G(j) divide G.toList)._2
        if (!s.isZero) {
          t += 1
          G +:= s
          B = B ++ (for {i <- 0 until t} yield (i, t))
        }
        B = B.tail
      }
    }
    G
  }
  def of[F](fs: Polynomial[F]*)(implicit F: Field[F]) = Buchberger(fs)
}

object xxx extends App {
  println(GroebnerBasis.kpairs(4))
}
