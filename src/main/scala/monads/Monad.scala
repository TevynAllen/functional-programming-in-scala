package monads

import applicative.Applicative
import functionalparallelism.Par
import functionalparallelism.Par.Par
import parsing.Parsers
import propertybasedtesting.Gen
import purelyfunctionalstate.State
import stictnesslaziness.Stream

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))
}

/**
  * A monad is just an applicative functor with an additional combinator, join
  */
trait Monad[M[_]] extends Applicative[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] //obeys an associative law - x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](fab: M[A => B])(fa: M[A]): M[B] =
    flatMap(fab)(f => flatMap(fa)(a => f(a)))

  override def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  override def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  override def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldLeft(unit(List[A]()))((m, l) =>  map2(l, m)(_ :: _))

  override def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldLeft(unit(List[B]()))((m, a) => map2(f(a), m)((a, l) =>  a :: l))

  override def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  override def factor[A,B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((_, _))

  def cofactor[A,B](e: Either[M[A], M[B]]): M[Either[A, B]] = ???

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => g(f(a))

  def join[A](mma: M[M[A]]): M[A] =
    flatMap(mma)(a => a)

  /**
    * left identity - flatMap(unit(x))(f) == f(x)
    * right identity - flatMap(x)(unit) == x
    */

  def flatMap_[A,B](ma: M[A])(f: A => M[B]): M[B] =
    join(unit(ma))(f)

}

object Monad {

  import functionaldatastructures._
  import handlingerrors._

  val genMonad: Monad[Gen] = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)

    def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] =
      flatMap(ma)(f)
  }

  val parserMonad: Monad[Parsers] = new Monad[Parsers] {
    def unit[A](a: => A): Parsers[A] = unit(a)

    def flatMap[A, B](ma: Parsers[A])(f: A => Parsers[B]): Parsers[B] =
      flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] =
      Some(a)

    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream.apply(a)

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma flatMap f
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      List.flatMap(ma)(f)
  }
}

case class Id[A](value: A) {
  def unit[A](a: => A): Id[A] = Id(a)
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

/**
  * it would be really repetitive if we had to manually write a separate Monad instance for each specific state type.
  * Unfortunately, Scala does not allow us to use underscore syntax to simply say State[Int, _]
  * to create an anonymous type constructor like we create anonymous functions with the underscore syntax.
  * But instead we can use something similar to lambda syntax at the type level.
  * For example, we could have declared IntState directly inline like this:
  */

object IntStateMonad extends Monad[({type IntState[A] = State[Int, A]})#IntState] {
  override def unit[A](a: => A): State[Int, A] = ???

  override def flatMap[A, B](ma: State[Int, A])(f: A => State[Int, B]): State[Int, B] = ???
}

/**
  * This syntax can be a little jarring when you first see it.
  * But all we are doing is declaring an anonymous type within parentheses.
  * This anonymous type has, as one of its members, the type alias IntState, which looks just like before.
  * Outside the parentheses we are then accessing its IntState member with the # syntax.
  * Just like we can use a "dot" (.) to access a member of an object at the value level,
  * we can use the # symbol to access a type member
  */

/**
  * A type constructor declared inline like this is often called a type lambda in Scala.
  * We can use this trick to partially apply the State type constructor and declare a StateMonad trait.
  * An instance of StateMonad[S] is then a monad instance for the given state type S.
  */
object StateMonad {
  def stateMonad[S]: Monad[State[S, _]] = new Monad[({type lambda[x] = State[S,x]})#lambda] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma.flatMap(f)
  }
}

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R]: Monad[Reader[R, _]] = new Monad[({type f[x] = Reader[R,x]})#f] {
    override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

    override def flatMap[A, B](ma: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(ma.run(r)).run(r))
  }
}
