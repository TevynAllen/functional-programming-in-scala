package monads

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

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] //obeys an associative law - x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldLeft(unit(List[A]()))((m, l) =>  map2(l, m)(_ :: _))

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldLeft(unit(List[B]()))((m, a) => map2(f(a), m)((a, l) =>  a :: l))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  def factor[A,B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((_, _))

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

object StateMonad {
  def stateMonad[S] = new Monad[({type lambda[x] = State[S,x]})#lambda] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma.flatMap(f)
  }
}

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

    override def flatMap[A, B](ma: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(ma.run(r)).run(r))
  }
}
