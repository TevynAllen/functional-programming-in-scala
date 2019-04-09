package monoid

/**
  * A monoid is an algebra which consists of associativity and idenitity laws called the monoid laws
  * A monoid consists of
  * - Some type A
  * - A binary associative operation that takes two values of type A and combines them into one
  * - A value of type A that is an identity for that operation
  */
trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A

  //string monoid
  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    def zero = ""
  }

  //list concatenation also forms a monoid
  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    def zero = Nil
  }

  //EXERCISE 1: Give Monoid instances for integer addition and multiplication as well as the Boolean operators
  val intAddition: Monoid[Int] = new Monoid[Int] {
     def op(a1: Int, a2: Int): Int = a1 + a2
     def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 0
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero: Boolean = true
  }


}
