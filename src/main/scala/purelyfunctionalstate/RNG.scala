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


}
