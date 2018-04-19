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

}