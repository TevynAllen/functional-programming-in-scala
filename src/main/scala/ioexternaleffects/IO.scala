package ioexternaleffects

import monads.Monad

/**
  * Its usage is important mainly because it clearly separates pure code from impure code,
  * forcing us to be honest about where interactions with the outside world are occurring
  * and also encouraging the beneficial factoring of effects
  */

/**
  * - IO computations are ordinary values - We can store them in lists, pass them to functions, create them dynamically, etc.
  *                                         Any common pattern we notice can be wrapped up in a function and reused.
  *
  * - Reifying IO computations as values means we can craft a more interesting interpreter than the simple run-based "interpreter"
  *   baked into the IO type itself
  */

trait IO[+A] { self =>
  def run: A
  def map[B](f: A => B): IO[B] = new IO[B] { def run = f(self.run)}
  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
    override def run: B = f(self.run).run
  }
}



object IO extends Monad[IO] {
  def unit[A](a: => A): IO[A] = new IO[A]  { def run: A = a}

  def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f

  def apply[A](a: => A): IO[A] = unit(a)

}

object IO1 {

  trait IO[F[_], +A]

  case class Pure[F[_], +A](get: A) extends IO[F, A]

  case class Request[F[_], I, +A](expr: F[I], receive: I => IO[F, A]) extends IO[F, A]

  trait Console[A]

  case object ReadLine extends Console[Option[String]]

  case class PrintLine(s: String) extends Console[Unit]

  def monad[F[_]] = new Monad[({type f[a] = IO[F, a]})#f] {
    override def unit[A](a: => A): IO[F, A] = ???

    override def flatMap[A, B](ma: IO[F, A])(f: A => IO[F, B]): IO[F, B] = ???
  }
}

object TrampolineIO {

  trait Trampoline[+A]
  case class Done[+A](get: A) extends Trampoline[A]
  case class More[+A](force: () => Trampoline[A]) extends Trampoline[A]
  case class Bind[A,+B](force: () => Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]

  object Trampoline {

    @annotation.tailrec
    def run[A](t: Trampoline[A]): A = t match {
      case Done(a) => a
      case More(f) => run(f())
      case Bind(a, f) => ???
    }
    
    val monadTrampoline = new Monad[Trampoline] {
      def unit[A](a: => A): Trampoline[A] = Done(a)
      def flatMap[A,B](a: Trampoline[A])(f: A => Trampoline[B]): Trampoline[B] = a flatMap f
      def more[A](a: => Trampoline[A]) =
        More(() => ()).flatMap { _ => a }

    }
  }
  
}