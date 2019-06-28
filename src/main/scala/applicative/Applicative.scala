package applicative

import monads.Functor

/**
  * applicative functors, which are simpler and more general than monads.
  */

trait Applicative[F[_]] extends Functor[F] {
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, x) => f(x))

  def unit[A](a: A): F[A]


  def apply[A,B](oa: Option[A], oab: Option[A => B]): Option[B] =
    (oa, oab) match {
      case (Some(a), Some(f)) => Some(f(a))
      case _ => None
    }

  /**
    *  difference into words is to say that applicative operations preserve structure while
    *  monadic operations may alter a structure
    */
}
