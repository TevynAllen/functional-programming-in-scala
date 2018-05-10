package stictnesslaziness

import Stream._

/**
  * : => denotes a non-strict function(lazy function)
  * Arguments that we would like to pass unevaluated have => right before their type
  */

trait Stream[+A] {
  /**
    * EXERCISE 1
    */
  def toList: List[A] = {
    def acc(s: Stream[A], l: List[A]): List[A] = s match {
      case Cons(h, t) => acc(t(), h() :: l)
      case _ => l
    }

    acc(this, List())
  }

  /**
    * EXERCISE 2
    */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n != 0 => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  /**
    * EXERCISE 3
    */

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h) => cons(h(), t().takeWhile(p))
    case Cons(h, _) if p(h) => cons(h(), empty)
    case _ => Empty
  }

  // The arrow `=>` in front of the argument type `B` means that the function `f`
  // takes its second argument by name and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h, t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /**
    * EXERCISE 4
    */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  /**
    * EXERCISE 5
    */
  def takeWhileWithFoldRight(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => {
      if (f(h)) cons(h, t) else
        empty
    })

  /**
    * EXERCISE 6
    */
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
    if(f(h)) cons(h, t) else t)


  // the B>:A parameter on these functions indicates that B must be a supertype of A.
  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  val ones: Stream[Int] = cons(1, ones)

  /**
    * EXERCISE 7
    */
  def constant[A](a: A): Stream[A] = {
    lazy val c = cons(a, c)
    c
  }

  /**
    * EXERCISE 8
    */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /**
    * EXERCISE 9
    */
  def fibs: Stream[Int] = {
    def go(f1: Int, f2: Int): Stream[Int] = {
      cons(f1, go(f2, f1+f2))
    }
    go(0, 1)
  }

  /**
    * EXERCISE 11
    */

  def fibsWithUnfold = unfold((0, 1)){
    case (f1, f2) => Some((f1, (f2, f1 + f2)))
  }

  def fromWithUnfold(n: Int) = unfold(n)(i => Some((i, i + 1)))

  def constantWithUnfold[A](a: A) = unfold(a)(a => Some((a, a)))

  def onesWithUnfold = unfold(1)(one => Some((one, one)))

  /**
    * EXERCISE 12
    */

  def mapWithUnfold[B](f: A => B): Stream[B] =
    unfold(this){
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeWithUnfold(n: Int): Stream[A] =
    unfold((this, n)){
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case (Cons(h, _), 1) => Some((h(), (empty, 0)))
      case _ => None
    }

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] =
    unfold(this){
      case Cons(h, t) if(p(h)) => Some((h(), t()))
      case _ => None
    }

  def zipWithUnfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)){
      case (Cons(h, t), Cons(h2, t2)) => Some((f(h(), h2()), (t(), t2())))
      case _ => None
    }

  def zipAllWithUnfold[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s2)){
      case (Cons(h, t), Cons(h2, t2)) => Some(f(Some(h()), Some(h2())) -> (t(), t2()))
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h2, t2)) => Some(f(Option.empty[A], h2()) -> (empty[A], t2()))
      case _ => None
    }

  /**
    * EXERCISE 13
    */

  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean = ???

  /**
    * EXERCISE 14
    */

  def tails: Stream[Stream[A]] = ???
    unfold(this){
      case Empty => None
      case s => Some((s, s ))
    }


}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {

  lazy val head = hd
  lazy val tail = tl
  Cons(() => head, () => tail)

}

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))


  /**
    * EXERCISE 10
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, t)) => cons(h, unfold(t)(f)) // it applies the unfold function to the tail of the
      case None => empty
    }
}