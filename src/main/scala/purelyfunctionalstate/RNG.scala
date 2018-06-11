package purelyfunctionalstate

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt: (Int, RNG) = {
      val seed2: Long = (seed*0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }

  // EXERCISE 1
  def positiveInt(rng: RNG): (Int, RNG) = {
    val (int, r) = rng.nextInt
    (if(int < 0) -(int + 1) else int, r)
  }

  // EXERCISE 2
  def double(rng: RNG): (Double, RNG) = {
    val (pi, rng) = positiveInt(rng)
    (pi / (Int.MaxValue.toDouble + 1), rng)
  }

  //EXERCISE 3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (pi, rng1) = positiveInt(rng)
    val (d, rng2) = double(rng1)
    ((pi, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((int, d), r) = intDouble(rng)
    ((d, int), r)
  }


  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)

    ((d1, d2, d3), rng3)
  }

  //EXERCISE 4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (List(), rng)
    else {
      val (int1, rng1) = rng.nextInt
      val (int2, rng2) = ints(count - 1)(rng1)

      (int1 :: int2, rng2)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  /**
    * RNG-transition is the unit action, which passes the RNG state through without using it,
    * always returning a constant value rather than a random value.
    */
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  //map, for transforming the output of a state action without modifying the state itself.

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // creating a map and unit functions has made RNG a functor type

  //EXERCISE 5 - Use map to generate an Int between 0 and n

  def positiveMax(n: Int): Rand[Int] = map(ints(n))(i => i.max)

  // EXERCISE 6

  def doubleWithMap: Rand[Double] = map(positiveInt)(_ / (Int.MaxValue.toDouble + 1))

  //EXERCISE 7

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def intDoubleWithMap[A, B](ra: Rand[A], rb: Rand[B]): Rand[(Int, Double)] = ???

  def doubleIntWithMap: Rand[(Double, Int)] = ???

  //EXERCISE 8
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((rand, ba) => map2(rand, ba)((r, b) => r :: b))

  //EXERCISE 9
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  //??? need to do testing
  def positiveIntWithFlatMap: Rand[Int] = flatMap(int){ i =>
    if(i < 0) unit(i.abs) else unit(i)
  }

  //EXERCISE 10
  def mapUsingFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2UsingFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
}

//  type State[S,+A] = S => (A, S)
case class State[S,+A](run: S => (A,S)) {
  //EXERCISE 11
  def map[B](f: A => B): State[S, B] = State(s => {
      val (a, s1) = run(s)
      (f(a), s1)
    })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S,B]): State[S,B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S,A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List[A]()))((st, b) => st.map2(b)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](newState: S): State[S, Unit] = State(_ => ((), newState))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Dispenser {

  def simulateMachine(inputs: List[Input]): State[Machine, Int] = ???
}