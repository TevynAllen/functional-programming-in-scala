package localeffectsandmutablestate

/**
  * This new data type will employ Scala's type system to gain two static guarantees.
  * That is, we want code that violates these invariants to not complile:
  *
  * 1. A mutable object can never be observed outside of the scope in which it was created.
  * 2. If we hold a reference to a mutable object, then nothing can observe us mutating it.
  */

/**
  * We will call this new local effects monad ST, which could stand for "State Thread", "State Transition", "State Token", or "State Tag".
  * It's different from the State monad in that its run method is protected,
  * but otherwise its structure is exactly the same.
  */
sealed trait ST[S,A] { self =>

  protected def run(s: S): (A, S)

  def map[B](f: A => B) = new ST[S,B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }

}

object ST {
  def apply[S, A](a: => A): ST[S, A] = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S) = (memo, s)
    }
  }
}

sealed trait STRef[S,A] {
  protected var cell: A
  def read: ST[S,A] = ST(cell)
  def write(a: => A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    } }
}
object STRef {
  def apply[S,A](a: A): ST[S, STRef[S,A]] = ST(new STRef[S,A] {
    var cell = a })
}