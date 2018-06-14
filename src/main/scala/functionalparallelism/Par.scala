package functionalparallelism

sealed trait Par[A]

object Par {

  def unit[A](a: => A): Par[A] = ???
  def get[A](a: Par[A]): A = ???

  def sum(as: IndexedSeq[Int]): Int =
    if(as.size <= 1) as.headOption getOrElse 0
    else {
      val (l, r) = as.splitAt(as.length/2)
      val sumL: Par[Int] = Par.unit(sum(l))
      val sumR: Par[Int] = Par.unit(sum(r))

      Par.get(sumL) + Par.get(sumR)
    }
}