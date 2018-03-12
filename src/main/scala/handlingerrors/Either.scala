package handlingerrors

/**
  * Either data type has many purposes -
  * 1. lets us track the reason for the failure
  * 2. to encode one of two possibilities, in cases where it isn't worth defining a fresh data type
  */


sealed trait Either[+E, +A] {
  /**
    *EXERCISE 7 - Implement versions of map, flatMap, orElse, and map2
    * on Either that operate on the Right value.
    */


  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

  def orElse[EE >: E, B >:A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }

  // Either can be used in for comprehensions
  // meaning if it contains Left(...) the yield result would be Left(..)
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      bb <- b
    } yield f(a, bb)
}
case class Left[+E](value: E) extends Either[E, Nothing] // failure case
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if(xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  /**
    * EXERCISE 8 - Implement sequence and traverse for Either.
    */

  // combine a list of Either into one Either containing a list of all the Right values in the list
  // if the original list contains Left(..) once the result of the function should be Left...
  // otherwise the result should be Right with the list of all values
  def sequence[E,A](es: List[Either[E, A]]): Either[E, List[A]] = ???


  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case h :: t => (f(h) map2 traverse(t)(f))((a, b) => a :: b)
      case Nil => Right(Nil)
    }

}