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
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = ???
}
