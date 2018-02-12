package main.scala.gettingstarted

import scala.annotation.tailrec

object MyModule {

  //abs method is a pure function that takes an integer and returns an integer value
  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  //formatAbs is also a pure function
  //formatAbs is also private meaning it can not be called outside the MyModule object
  private def formatAbs(x: Int): String = {
    val msg = "The absolute value of %d is %d."
    msg.format(x, abs(x))
  }

  // example of writing a purely functional loop without mutation
  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  /** EXERCISE 1
    *
    * Write a function to get the nth Fibonacci number.
    * The first two Fibonacci numbers are 0 and 1, and the next number is always the sum of the previous two.
    * Your definition should use a local tail-recursive function.
    */
  //0,1,1,2,3,5,8,13,21
  def fib(n: Int): Int = {
    @tailrec
    def tailFunction(n: Int, firstNumber: Int, secondNumber: Int): Int = {
      if (n == 0) firstNumber
      else tailFunction(n - 1, secondNumber, secondNumber + firstNumber)
    }
    tailFunction(n, 0, 1)
  }


  /**
    *
    * this is a higher order function because it is a fuction which takes another function
    * in its parameters
    *
    */
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }


  def binarySearch(ds: Array[Double], key: Double): Int = {
    @tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2: Int = (low + high) / 2
        val d = ds(mid2)
        if (d == key) mid2
        else if (d > key) go(low, mid2, mid2 - 1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, ds.length - 1)
  }


  /** EXCERSICE 2
    *
    *
    */

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(n: Int): Boolean =
      if (n >= as.length) true
      else if (gt(as(n), as(n + 1))) false
      else go(n + 1)

    go(0)
  }


  /** EXERCISE 3
    *
    */


  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)


  /** EXERCISE 4
    *
    * A => (B => C) is the same as (A => B => C)
    * */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)


  /** EXERCISE 5
    * */

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a: A, b: B) => f(a)(b)


  /** EXERCISE 6
    *
    */

  def compose[A,B,C](f: B => C, g: A => B): A => C =
  a => f(g(a))


  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(fib(3))
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("increment", 7, (x: Int) => x + 1))
    println(formatResult("increment2", 7, (x) => x + 1))
    println(formatResult("increment3", 7, x => x + 1))
    println(formatResult("increment4", 7, _ + 1))
    println(formatResult("increment5", 7, x => {
      val r = x + 1; r
    }))
  }
}
