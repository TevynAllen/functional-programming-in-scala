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