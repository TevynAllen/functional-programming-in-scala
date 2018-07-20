package functionalparallelism

import java.util.concurrent.{Future, ExecutorService, TimeUnit}

sealed trait Par[A] {
}

object Par {

  type Par[A] = ExecutorService => Future[A]

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

  def sum(as: IndexedSeq[Int]): Par[Int] =
    if(as.size <= 1) Par.unit(as.headOption getOrElse 0)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sum(l), sum(r))(_ + _)
    }

}