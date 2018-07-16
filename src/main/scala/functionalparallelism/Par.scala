package functionalparallelism

import java.util.concurrent.{Callable, TimeUnit}

sealed trait Par[A] {
}

object Par {

  type Par[A] = ExecutorService => Future[A]

  //for taking an unevaluated A and returning a
  // parallel computation that yields an A
  def unit[A](a: => A): Par[A] = ???

  def async[A](a: => A): Par[A] = fork(unit(a))

  //for extracting the resulting value from a parallel computation
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def fork[A](a: => Par[A]): Par[A] = ???

  def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) =>
      val a = p1(es)
      val b = p2(es)

    ??? //  fork(unit(f(a.get, b.get)))
  }

  /*def sum(as: IndexedSeq[Int]): Int =
    if(as.size <= 1) as.headOption getOrElse 0
    else {
      val (l, r) = as.splitAt(as.length/2)
      val sumL: Par[Int] = Par.unit(sum(l))
      val sumR: Par[Int] = Par.unit(sum(r))

      Par.get(sumL) + Par.get(sumR)
    }*/

  def sum(as: IndexedSeq[Int]): Par[Int] =
    if(as.size <= 1) Par.unit(as.headOption getOrElse 0)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sum(l), sum(r))(_ + _)
    }



}

class ExecutorService {
  def submit[A](a: Callable[A]): Future[A] = ???
}

trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}