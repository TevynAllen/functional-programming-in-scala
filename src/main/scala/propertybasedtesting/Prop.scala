package propertybasedtesting

import propertybasedtesting.Prop.{FailedCase, SuccessCount}
import purelyfunctionalstate.{RNG, State}

case class Gen[+A](sample: State[RNG, A], exhaustive: Option[Stream[A]]) {

  //  type Gen[A] = State[RNG, A]
  //type Gen[+A] = (State[RNG, A], List[A])

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a), None)

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean), None)

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)), None)

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  def uniform: Gen[Double] =
    Gen(State(RNG.double), Some(Stream.apply()))

  def choose(i: Double, j: Double): Gen[Double] =
    Gen(State(RNG.double).map(d => i + d % j), Some(Stream.apply(i)))

  //EXERCISE 7
  def map[A, B](f: A => B): Gen[B] =
    Gen(sample.map(a => f(a)), None)

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f), None)

  //EXERCISE 8
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample), None)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(i => this.listOfN(i, this))

  //EXERCISE 10
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if(b) g1 else g2)

  // EXERCISE 11
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Prob = g1._2 / (g1._2 + g2._2)

    Gen(State(RNG.double).flatMap(d => if(d < g1Prob) g1._1.sample else g2._1.sample), None)
  }

}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.positiveInt).map(n => start + n % stopExclusive), None)
  }

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] = {
    val choice = choose(from, to)
    val num = ???
    ???
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

trait Status
case object Proven extends Status
case object Unfalsified extends Status

trait Prop {
  def check: Either[FailedCase, SuccessCount]

  def &&(p: Prop): Prop = ???

  def run: Either[FailedCase, (Status, SuccessCount)]
}