package handlingerrors

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  // : => B type annotation indicated the argument will not be evaluated until is is needed by the function
  // B >: A indicates that the B must be a supertype of A
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
  //The first will compute the mean of the data set
    mean(xs).flatMap(m =>
      //the second will compute the mean squared difference from this mean.
      mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  /**
    * EXERCISE 3: bothMatch is an instance of a more general pattern.
    * Write a generic function map2,
    * that combines two Option values using a binary function.
    */

  def map2_1[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(x), Some(xs)) => Some(f(x, xs))
    }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(x => b map(xs => f(x, xs)))

  /**
    * EXERCISE 4
    * */
  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = ???

  /**
    * EXERCISE 5 - Write a function sequence,
    * that combines a list of Options into one option containing a list of all the Some values in the original list.
    * If the original list contains None even once, the result of the function should be None,
    * otherwise the result should be Some with a list of all the values.
    */

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => None
      case h :: t => h flatMap(head => sequence(t) map(tt => head :: tt))
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???

}
