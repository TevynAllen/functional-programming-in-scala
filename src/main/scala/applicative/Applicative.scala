package applicative

import java.util.Date

import monads.Functor

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

  val v: F[List[Int]] = unit(List(1))
  val a = apply(unit(x => x))(v) == v

  val aa: F[List[Int]] = map(v)(x => x)

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
    * - 2. composition law - preserve function composition
    */

}