package functionalparallelism

import java.util.concurrent.{Future, ExecutorService, TimeUnit}

object Par {

  type Par[A] = ExecutorService => Future[A]

  /**
    * EXERCISE 3
    */
  //for taking an unevaluated A and returning a
  // parallel computation that yields an A
  def unit[A](a: => A): Par[A] = es => Fut(a)

  def async[A](a: => A): Par[A] = fork(unit(a))

  //for extracting the resulting value from a parallel computation
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def fork[A](a: => Par[A]): Par[A] = ???

  def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) =>
      val a = p1(es).get()
      val b = p2(es).get()

    Fut(f(a, b))
  }

  case class Fut[A](a: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = ???

    override def isCancelled: Boolean = ???

    override def isDone: Boolean = ???

    override def get(): A = ???

    override def get(timeout: Long, unit: TimeUnit): A = ???
  }

  // EXERCISE 4
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => unit(f(a))

  def sortPar(l: Par[List[Int]]): Par[List[Int]] =
    map2(l, unit(()))((a, _) => a.sorted)

  def map[A, B](fa: Par[A])(f: A => B): Par[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def sortParWithMap(l: Par[List[Int]]) = map(l)(_.sorted)

  //EXERCISE 5
  def product[A, B](fa: Par[A], fb: Par[B]): Par[(A, B)] = {
    es =>
      val a = fa(es).get()
      val b = fb(es).get()

      Fut((a, b))
  }

  def nonPrimitiveMap[A, B](fa: Par[A])(f: A => B): Par[B] = {
   es =>
      val a = fa(es).get()
      Fut(f(a))
  }

  // EXERCISE 6: Note that we could always just write parMap as a new primitive.
  // See if you can implement it this way.
  // Remember that Par[A] is simply an alias for ExecutorService => Future[A].
  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = l.map(asyncF(f))
    sequence(fbs)
  }

  // EXERCISE 7
  def sequence[A](l: List[Par[A]]): Par[List[A]] = l match {
    case Nil => unit(Nil)
    case h :: t => map2(h, fork(sequence(t)))(_ :: _)
  }

  def sequenceWithFold[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((head, tail) => map2(head, tail)(_ :: _))

  //EXERCISE 8
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t))

  def parFilter2[A](l: List[A])(f: A => Boolean): Par[List[A]] =
    parMap(l)(a => if (f(a)) List(a) else Nil)

  def sum(as: IndexedSeq[Int]): Par[Int] =
    if(as.size <= 1) Par.unit(as.headOption getOrElse 0)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sum(l), sum(r))(_ + _)
    }

}