package applicative

import java.util.Date

import monads.{Functor, Monad, StateMonad}
import monoid.{Foldable, Monoid}
import purelyfunctionalstate.State

/**
  * When using Monads we lose some compositionality.
  * We can reclaim it by instead using applicative functors, which are simpler and more general than monads.
  */

trait Applicative[F[_]] extends Functor[F] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, x) => f(x))

  //sometimes called pure
  def unit[A](a: A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(f))((a, b) => b(a))

  //simplified into map2(a, unit(f))(_(_))

  def apply[A, B](oa: Option[A])(oab: Option[A => B]): Option[B] =
    (oa, oab) match {
      case (Some(a), Some(f)) => Some(f(a)) // The action of apply is to apply the function inside one argument to the value inside the other.
      case _ => None
    }

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {

      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))

    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: A): F[G[A]] = self.unit(G.unit(a))

      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa, fb)((a, b) => G.map2(a, b)(f))
    }
  }

  /** Difference between functors and applicative functors
    *
    * Both apply and map are a kind of function application(applying a function to an argument to obtain a value) in a context
    * Difference between apply and map
    * - for apply - (e.g. Option) if the second argument to apply is None then there is no function at all
    *  - for map - (e.g. Option) the function (higher order function) must exists independently of the context
    */


  /** Differnce between monads and applicative functors
    *
    * All monads are applicative functors
    * However not all applicative functors are monads
    */

  /**
    * difference into words is to say that applicative operations preserve structure while
    * monadic operations may alter a structure
    * e.g.
    *  - if you map over a list with three elements the result will have three elements
    *  - if you flapMap over a list with three elements,
    * you may get many more since each function application can introduce a whole list of new values
    */

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(a => a)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((a, b) => (a, b))

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
  ofa.foldRight(unit(Map.empty[K, V])) { case ((k, f), acc) =>
    map2(acc, f)((a, v) => a + (k -> v))
  }
}

sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {

  def validationApplicative[E]: Applicative[Validation[E, _]] = new Applicative[({type lambda[x] = Validation[E, x]})#lambda] {
    override def unit[A](a: A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
      (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (Failure(h, t), Failure(hh, tt)) => Failure(h, t ++ Vector(hh) ++ tt)
        case (e@Failure(_, _), _) => e
        case (_, e@Failure(_, _)) => e
      }

    override def apply[A, B](fab: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] =
      map2(fab, fa)((ab, a) => ab(a))
  }

  case class WebForm(name: String, birthdate: Date, phoneNumber: String)

  def validName(name: String): Validation[String, String] =
    if (name != "")
      Success(name)
    else Failure("Name cannot be empty", Vector.empty)

  def validBirthdate(birthdate: String): Validation[String, Date] =
    try {
      import java.text._
      Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthdate))
    } catch {
      Failure("Birthdate must be in the form yyyy-MM-dd", List())
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
    else Failure("Phone number must be 10 digits")

  def validWebForm(name: String,
                   birthdate: String,
                   phone: String): Validation[String, WebForm] =
    apply(apply(apply((WebForm(_, _, _)).curried)(
      validName(name)))(
      validBirthdate(birthdate)))(
      validPhone(phone))


  /**
    * The Applicative Laws
    *
    * -- The should obey the functor law - map(v)(x => x) == v which is the same applicative law of identity -
    * -- 1. The law of identity where the unit preserve identities for instance - apply(unit(x => x))(v) == v
    *
    * - 2. composition law - preserve function composition and using apply and unit
    * we can lift function composition in two different ways
    *
    * f - F[A => B]
    * g - F[B => C]
    * x - F[A]
    *
    * apply(apply(apply(unit(a => b => c => a(b(c)))))(g))(x) == apply(f)(apply(g)(x))
    * they are both applying g to x then f to the result of that
    *
    *  3. homomorphism and interchange law establishes the relationship between unit and apply
    *
    *  3.1 - homomorphism law states that passing a function and a value through unit is the same
    * as passing the result of regular application through unit
    *
    * apply(unit(f))(unit(x)) == unit(f(x))
    *
    * passing a function to a unit and then apply is the same as calling map
    *
    * map(unit(x))(f) == unit(f(x))
    *
    *
    *  3.2 - the interchange law states that unit should have the same effect whether applied to the first or second argument of apply
    *
    * apply(u)(unit(y)) == apply(unit(_(y)))(u)
    *
    * it can be stated in a different way showing a relationship between map2 and map
    *
    * map2(u, unit(y))(_(_)) == map(u)(_(y))
    *
    * ________
    *
    * Overall laws are sanity checks that proves that the algerbas work in the way we would expect
    *
    */

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) = new Applicative[({ type f[x] = Const[M, x]})#f] {
    override def unit[A](a: A): Const[M, A] = M.zero

    override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
  }

  /**
    * applicative functors are sometimes called monoidal functors
    *  - operations of a monoid map directly onto operations of an applicative
    */
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  //traverse preserves the original structure
  def traverse[M[_]: Applicative, A, B](fa: F[A])(f: A => M[B]): M[F[B]]
    ///sequence(map(fa)(f))

  def sequence[M[_]: Applicative, A](fma: F[M[A]]): M[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A

  val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

 override def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) = new Applicative[({ type f[x] = Const[M, x]})#f] {
    override def unit[A](a: A): Const[M, A] = M.zero

    override def apply[A, B](m1: M)(m2: M): Const[M, B] = M.op(m1, m2)

  }

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type f[x] = State[S, x]})#f, A,B](fa)(f)(StateMonad.stateMonad)

}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse {

  val traverseList: Traverse[List] = new Traverse[List] {
    override def traverse[M[_], A, B](fa: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] =
      fa.foldLeft(M.unit(List[B]()))((acc, la) => M.map2(f(la), acc)((b, l) => b :: l))
  }

  val traverseOption: Traverse[Option] = new Traverse[Option] {
    override def traverse[M[_], A, B](fa: Option[A])(f: A => M[B])(implicit M: Applicative[M]): M[Option[B]] =
      fa match {
        case Some(v) => M.map(f(v))(a => Some(a))
        case None => M.unit(None)
      }
  }

  val traverseTree: Traverse[Tree] = new Traverse[Tree] {
    override def traverse[M[_], A, B](fa: Tree[A])(f: A => M[B])(implicit M: Applicative[M]): M[Tree[B]] =
      M.map2(f(fa.head), traverseList.traverse(fa.tail)(a => traverse(a)(f)))((a, b) => Tree(a, b))
  }

}