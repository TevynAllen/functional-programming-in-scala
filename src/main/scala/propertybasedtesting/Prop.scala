package propertybasedtesting

import propertybasedtesting.Prop.{FailedCase, SuccessCount}
import purelyfunctionalstate.{RNG, State}

case class Gen[+A](sample: State[RNG, A], exhaustive: Option[Stream[A]]) {

  //  type Gen[A] = State[RNG, A]
  //type Gen[+A] = (State[RNG, A], List[A])

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a), None)

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean), None)

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = ???

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.positiveInt).map(n => start + n % stopExclusive), None)
  }

  def uniform: Gen[Double] =
    Gen(State(RNG.double), Some(Stream.apply()))

  def choose(i: Double, j: Double): Gen[Double] =
    Gen(State(RNG.double).map(d => i + d % j), Some(Stream.apply(i)))

  def map[A, B](f: A => B): Gen[B] =
    Gen(sample.map(a => f(a)), None)

  def map2[A, B](f: A => B): Gen[B] =
    Gen(???)

  }

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

trait Prop {
  def check: Either[FailedCase, SuccessCount]

  def &&(p: Prop): Prop = ???
}