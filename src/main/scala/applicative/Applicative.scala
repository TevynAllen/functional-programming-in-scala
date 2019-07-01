package applicative

import monads.Functor

/**
  * When using Monads we lose some compositionality.
  * We can reclaim it by instead using applicative functors, which are simpler and more general than monads.
  */

trait Applicative[F[_]] extends Functor[F] {
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, x) => f(x))

  //sometimes called pure
  def unit[A](a: A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(f))((a, b) => b(a))
  //simplified into map2(a, unit(f))(_(_))

  def apply[A,B](oa: Option[A])(oab: Option[A => B]): Option[B] =
    (oa, oab) match {
      case (Some(a), Some(f)) => Some(f(a)) // The action of apply is to apply the function inside one argument to the value inside the other.
      case _ => None
    }

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(a => a)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb)((a, b) => (a, b))

  /**
    * Both apply and map are kind of function application(applying a function to an argument to obtain a value) in a context
    * Difference between apply and map
    * - for apply - (e.g. Option) if the second argument to apply is None then there is no function at all
    *  - for map - (e.g. Option) the function (higher order function) must exists independently of the context
    */



  /**
    *  difference into words is to say that applicative operations preserve structure while
    *  monadic operations may alter a structure
    * e.g.
    *  - if you map over a list with three elements the result will have three elements
    *  - if you flapMap over a list with three elements,
    *    you may get many more since each function application can introduce a whole list of new values
    */
}
