package propertybasedtesting

import propertybasedtesting.Prop.{FailedCase, Result, SuccessCount, TestCases}
import purelyfunctionalstate.{RNG, State}

//page 134
case class Prop(run: (TestCases, RNG) => Result) {
  def check: Either[FailedCase, SuccessCount] = ???

  def &&(p: Prop): Prop = Prop { (n, rng) =>
    run(n, rng) match {
      case Right(_) => p.run(n, rng)
      case x => x.left.e
    }
  }

  def ||(p: Prop): Prop = Prop { (n, rng) =>
    run(n, rng) match {
      case Right(_) => ???
    }
  }

}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type Result = Either[FailedCase, (Status, SuccessCount)]

  def run: Either[FailedCase, (Status, SuccessCount)] = ???

}

case class Gen[+A](sample: State[RNG, A], exhaustive: Option[Stream[A]]) {

  //  type Gen[A] = State[RNG, A]
  //type Gen[+A] = (State[RNG, A], List[A])

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a), None)

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean), None)

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)), None)

  /*def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) => {
    def go(i: Int, j: Int, s: Stream[Option[A]], onEnd: Int => Result):
    Result =
      if (i == j) Right((Unfalsified, i))
      else s.uncons match {
        case Some((Some(h), t)) =>
          try {
            if (f(h)) go(i + 1, j, s, onEnd) else Left(h.toString)
          }
          catch {
            case e: Exception => Left(buildMsg(h, e))
          }
        case Some((None, _)) => Right((Unfalsified, i))
        case None => onEnd(i)
      }

    go(0, n / 3, a.exhaustive, i => Right((Proven, i))) match {
      case Right((Unfalsified, _)) =>
        val rands = randomStream(a)(rng).map(Some(_))
        go(n / 3, n, rands, i => Right((Unfalsified, i)))
      case s => s
    }
  }
  }*/

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))


  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    "test case: " + s + "\n" +
      "generated an exception: " + e.getMessage + "\n" +
      "stack trace:\n" + e.getStackTrace.mkString("\n")

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
    boolean.flatMap(b => if (b) g1 else g2)

  // EXERCISE 11
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Prob = g1._2 / (g1._2 + g2._2)

    Gen(State(RNG.double).flatMap(d => if (d < g1Prob) g1._1.sample else g2._1.sample), None)
  }

  //EXERCISE 13
  def unsized: SGen[A] = SGen(_ => this)

  //EXERCISE 15
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen {
    i => listOfN(i, g)
  }

  //EXERCISE 17
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

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

trait SGen[+A]
case class Sized[+A](forSize: Int => Gen[A]) extends SGen[A]
case class Unsized[+A](get: Gen[A]) extends SGen[A]

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def map[A, B](f: A => B): SGen[B] = SGen {
    forSize(_).map(f)
  }
}

trait Status
case object Exhausted extends Status
case object Proven extends Status
case object Unfalsified extends Status
