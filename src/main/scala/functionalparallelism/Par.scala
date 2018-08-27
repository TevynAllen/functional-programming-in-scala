package functionalparallelism

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

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

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) =>
      val a = p1(es).get() //p1(es) will submit a Callable to the ExecutorService and get back a Future
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

  def parFilter3[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map asyncF((a: A) => if (f(a)) List(a) else List())
    map(sequence(pars))(_.flatten)
  }

  def sum(as: IndexedSeq[Int]): Par[Int] =
    if (as.size <= 1) Par.unit(as.headOption getOrElse 0)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sum(l), sum(r))(_ + _)
    }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  //identity function
  def id[A](a: A): A = a

  val x = 1
  val y = unit(x)
  map(unit(x))(id) == unit(id(x)) // substitute identity function for f
  map(unit(x))(id) == unit(x) // simplify
  map(y)(id) == y // substitute y for unit(x) on both sides

  /**
    * To get some insight into what this new law is saying,
    * let's think about what map cannot do. It cannot, say,
    * throw an exception and crash the computation before applying the function to the result
    * - Can you see why this violates the law?
    */

  //EXERCISE 9
  /**
    * map(map(y)(g))(f) == map(y)(f compose g)
    */

  //fork(x) == x  - fork should not affect the result of a parallel computation
  //fork(x) should 'do the same thing' as x, but asynchronously, in a logical thread separate from the main thread.


  def fork_simple[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] { // invoking submit inside a callable
      def call = a(es).get // we are blocking on the result of what we've submitted
    })


  //EXERCISE 11 - Can you show that any fixed size thread pool can be made to
  // deadlock given this implementation of fork?
  fork(fork(fork(y)))
  // considering if we had a fixed size thread pool executing multiple forks which
  // will mean there will be another deadlock since our current impl of fork will require at
  // least three threads in order to complete one fork

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)


  //EXERCISE 14
  def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    es => if (a(es).get()) ifTrue(es) else ifFalse(es)

  // alternatively could use run function to extract the result
  // a(es) == run(es)(a)

  //EXERCISE 15 - to be able to choose between two parallel computations based on the results of a first
  def choiceN[A](a: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val execPar = run(es)(a).get()
      run(es)(choices(execPar))
    }

  def choiceInTermsOfChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    choiceN(map(a)(bool => if (bool) 0 else 1))(List(ifTrue, ifFalse))

  //EXERCISE 16
  def choiceMap[A, B](a: Par[A])(choices: Map[A, Par[B]]): Par[B] =
    es => {
      val parKey = a(es).get()
      choices(parKey)(es)
    }

  //EXERCISE 17
  def flatMap[A, B](a: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val par = a(es).get()
      choices(par)(es)
    }

  def choiceNWithChooser[A](a: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(a)(int => choices(int))

  def choiceWithChooser[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    flatMap(a)(bool => if (bool) ifTrue else ifFalse)

  //EXERCISE 18
  def join[A](a: Par[Par[A]]): Par[A] =
    es => a(es).get()(es)

}