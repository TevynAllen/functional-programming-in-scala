package functionaldatastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  /**
    * EXERCISE 25 - Write a function size that counts the number of nodes in a tree.
    */

  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  /**
    * EXERCISE 26 - Write a function maximum that returns the maximum element in a Tree[Int].
    */

  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(node) => node
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  /**
    * EXERCISE 27 - Write a function depth that returns the maximum path length from the root of a tree to any leaf.
    */
  def depth[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

  /**
    * EXERCISE 28 - Write a function map, analogous to the method of the same name on List, that modifies each element in a tree with a given function.
    */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  /**
    * EXERCISE 29 - Generalize size, maximum, depth, and map,
    * writing a new function fold that abstracts over their similarities
    */
  def fold[A, B](t: Tree[A])(f: (A, B) => B): B =
    t match {
      case Leaf(a) => f(a)
      case Branch(l, r) => ???
    }
}