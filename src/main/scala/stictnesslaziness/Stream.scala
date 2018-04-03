package stictnesslaziness

trait Stream[+A] {
  /**
    * EXERCISE 1
    */
  def toList: List[A] = {
    def acc(s: Stream[A], l: List[A]): List[A] = s match {
      case Cons(h, t) => acc(t(), h() :: l)
      case _ => l
    }

    acc(this, List())
  }

  /**
    * EXERCISE 2
    */
  def take(n: Int): Stream[A] = ???

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {

  lazy val head = hd
  lazy val tail = tl
  Cons(() => head, () => tail)

}

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

}