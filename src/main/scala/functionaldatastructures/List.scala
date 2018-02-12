package main.scala.functionaldatastructures

import main.scala.functionaldatastructures.List.flatMap


/**
  * adding a sealed in front of the trait means that all
  * implementations of the trait must be declared in this file
  */

sealed trait List[+A] // defines a polymorphic data type called List

/**
  * each data contrcutor of List is introduced with the keyword case
  * they both represent each of the two possible forms a list can take
  *
  * */

case object Nil extends List[Nothing] // data constructor for List meaning empty
case class Cons[+A](head: A, tail: List[A]) extends List[A] // data constructor which has an element "head" which cannot be empty and tail List which can be empty

object List { // List companion object

  /**
    * Both sum and product are similar the only difference
    * - is the value returned if the list is empty
    * - the operation to apply to combine results
    *
    * This means it can be generalized by pulling the subexpressions out into function arguments
    *
    * 1. If a subexpression refers to any local variables, turn the subexpression into a function that accepts these variables as arguments (e.g. foldRight)
    */


  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs) //1
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs) //1
  }

  def apply[A](as: A*): List[A] = //Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1, 2, 3)
  val total = sum(example)


  /**
    * EXERCISE 1 - What will the result of the following match expression be>
    */

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // 3
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  /**
    * EXERCISE 2 - Implement the function tail for "removing" the first element of a List.
    * Notice the function takes constant time. what are different choices you could make in your implmentation if the List is NIl?
    */

  def tail[A](x: List[A]): List[A] = {
   x match {
     case Cons(h, t) => t
     case Cons(h, Nil) => Nil
     case Nil => Nil
   }
  }

  /**
    * EXERCISE 3 - Generalize tail to the function drop, which removes the first n elements form a list
    */

  def drop[A](n: Int, x: List[A]): List[A] =
  if(n <= 0) x else
  x match {
    case Cons(_, t) => drop(n - 1, t)
    case Nil => Nil
  }

  /**
    * EXERCISE 4 - Implement dropWhile, which removes elements from the
    * List prefix as long as they match a predicate. Again, notice these functions take
    * time proportional only to the number of elements being dropped—we do not need
    * to make a copy of the entire List.
    */

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => l
    }
  }

  /**
    * EXERCISE 5 - Using the same idea, implement the function setHead for
    * replacing the first element of a List with a different value.
    */

  def setHead[A](h: A, l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, t) => Cons(h, t)
    }
  }

  // a function that adds all the elements of one list to the end of another
  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }


  /**
    * EXERCISE 6 - Implement a function, init, which returns a List consisting
    * of all but the last element of a List . So, given List(1,2,3,4), init will return List(1,2,3).
    * Why can't this function be implemented in constant time like tail ?
    */

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  /**
    * The function will take the value to return if the list is empty - z
    * and any function in the case of an non empty list, e.g. add - f
    */
  def foldRight[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(l: List[Int]) =
    foldRight(l, 0.0)(_ + _)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)

  /**
    * EXERCISE 7: Can product implemented using foldRight immediately halt the recursion and return 0.0
    * if it encounters a 0.0? Why or why not?
    */

  // It would not halt the recursion, the recursion will complete the result would be 0.0.
  // foldRight traverses the list to the end before computing the result

  /**
    * EXERCISE 8 - What do you think this says about the relationship between foldRight and the data constructors of List?
    */
  /**
    * foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_,_)
    * Cons(1, foldRight(List(2, 3), Nil: List[Int])(Cons(_,_)))
    * Cons(1, Cons(2, foldRight(List(3), Nil: List[Int])(Cons(_,_)))
    * Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil: List[Int])(Cons(_,_))))
    * Cons(1, Cons(2, Cons(3, Nil)))
    *
    *
    * It returns back the original list.
    * The relationship it has with the data constructor of List is that the Nil constructor is z
    * and Cons constructor is f. Since in this case z is Nil and Cons is given as f, it will return the original list
    */

  /**
    * EXERCISE 9 - Compute the length of a list using foldRight
    */

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, tail) => tail + 1)
  }

  /** EXERCISE 10
    *
    */

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  /**
    * EXERCISE 11
    * */
  def sum3(l: List[Int]) = foldLeft(l, 0.0)(_ + _)

  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = {
    foldLeft(l, 0)((t, _) => t + 1)
  }

  /**
    * EXERCISE 12 - Write a function that returns the reverse
    * of a list (so given List(1,2,3) it returns List(3,2,1)). See if you can write it using a fold
    */

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((b, t) => Cons(t, b))

  /**
    * EXERCISE 13 - Can you write foldLeft in terms of foldRight? How about the other way around?
    *
    */


  /**
    * EXERCISE 14 - Implement append in terms of either foldLeft or foldRight.
    *
    */

  def append[A](l1: List[A], l2:List[A]): List[A] =
    foldRight(l1, l2)((h, t) => Cons(h,t))

  /***
    * EXERCISE 15 - Write a function that concatenates a list of lists into a single list.
    * Its runtime should be linear in the total length of all lists.
    * Try to use functions we have already defined.
    */

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  /**
    * EXERCISE 16 - Write a function that transforms a list of integers by adding 1 to each element.
    */

  def transformInt(l: List[Int]): List[Int] =
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(h + 1, transformInt(t))
    }

  def transformIntWithFoldRight(l: List[Int]): List[Int] =
    foldRight(l, List(0))((h, t) => Cons(h + 1, t))

  /**
    * EXERCISE 17 - Write a function that turns each value in a List[Double] into a String.
    */

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h, t) => Cons(h.toString, t))

  /**
    * EXERCISE 18 - Write a function map, that generalizes modifying each element in a list while maintaining the structure of the list
    */

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h, t) => Cons(f(h), t))

  /**
    * EXERCISE 19 - Write a function filter that removes elements from a list unless they satisfy a given predicate. Use it to remote all odd numbers from a List[Int].
    */

  def filter[A](l: List[A])(f: A => Boolean):List[A] =
    foldRight(l, Nil:List[A])((h, t) => if(f(h)) Cons(h, t) else t)

  def filterOddNum(l: List[Int]): List[Int] =
    filter(l)(_ % 2 == 0)

  /**
    * EXERCISE 20 - Write a function flatMap,
    * that works like map except that the function given will return a list
    * instead of a single result, and that list should be inserted into the final resulting list.
    */

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil:List[B])((h, t) => append(f(h), t))

  /**
    * EXERCISE 21
    *
    */
  def flatMapToFilter[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)((h) => if (f(h)) List(h) else Nil)

  /**
    * EXERCISE 22
    */
  def mergeTwoList(l1: List[Int], l2: List[Int]): List[Int] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h, t), Cons(a, b)) => Cons(h + a, mergeTwoList(t, b))
    }

  /**
    *EXERCISE 23
    */
  def mergeTwoListWithFunction[A, B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h, t), Cons(a, b)) => Cons(f(h, a), mergeTwoListWithFunction(t, b)(f))
    }
}