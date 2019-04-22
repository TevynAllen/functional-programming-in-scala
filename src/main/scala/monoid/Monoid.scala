package monoid

import functionaldatastructures.{Branch, Leaf, Tree}
import monoid.Monoid.foldMapV
import propertybasedtesting.Prop

/**
  * A monoid is an algebra which consists of associativity and idenitity laws called the monoid laws
  * A monoid consists of
  * - Some type A
  * - A binary associative operation that takes two values of type A and combines them into one
  * - A value of type A that is an identity for that operation
  */


/**
  * a monoid is a type together with an associative binary operation (op)
  * which has an identity element (zero).
  */

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  //  def foldRight(z: A)(f: (A, A) => A): A
  //  def foldLeft(z: A)(f: (A, A) => A): A

  //string monoid
  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    def zero = ""
  }

  val words = List("Hic", "Est", "Index")
  val s = words.foldRight(stringMonoid.zero)(stringMonoid.op)
  val t = words.foldLeft(stringMonoid.zero)(stringMonoid.op)

  //list concatenation also forms a monoid
  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
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

  // EXERCISE 2: Give a Monoid instance for combining Options
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    def zero: Option[A] = None
  }

  //EXERCISE 3: A function having the same argument and return type is called an endofunction
  def EndoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 andThen a2

    def zero: A => A = (a: A) => a
  }

  def monoidLaws[A](m: Monoid[A]): Prop = ???

  ///EXERCISE 5 - Write a monoid instance for String that inserts spaces between words unless there already is one, and trims spaces off the ends of the result
  def wordsMonoid(s: String): Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String =
      if (a1 == zero) a2
      else if (a2 == zero) a1
      else a1.trim + " " + a2.trim

    def zero: String = " "
  }

  //EXERCISE 6 - Implement concatenate, a function that folds a list with a monoid
  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  //EXERCISE 7 - map over the list to turn it into a type that has a monoid instance
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  //EXERCISE 8 - write foldLeft and foldRight using foldMap

  def foldLeftMap[A, B](as: List[A])(z: B)(f: (B, A) => B): B = ???

  def foldRightMap[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, EndoMonoid[B])(a => b => f(a, b))(z)

  sealed trait WC

  case class Stub(chars: String) extends WC // we have not seen any complete words yet
  case class Part(lStub: String, words: Int, rStub: String) extends WC //

  //EXERCISE 9
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC =
      (a1, a2) match {
        case (Stub(a), Stub(aa)) => Stub(a + aa)
        case (Stub(a), Part(b, i, c)) => Part(a + b, i, c)
        case (Part(b, i, c), Stub(a)) => Part(b, i, c + a)
        case (Part(a, i, c), Part(aa, ii, cc)) => Part(a, i + (if ((c + aa).isEmpty) 0 else 1) + ii, cc)
      }

    def zero: WC = Stub("")
  }

  //EXERCISE 10
  def count(s: String): Int = ???

  //EXERCISE 11
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty)
      m.zero
    else if (v.length == 1)
      f(v(0))
    else {
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  //EXERCISE 17 - if types A and B are monoids, then the tuple type (A, B) is also a monoid (called their product)
  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(a1: (A, B), a2: (A, B)): (A, B) = (a.op(a1._1, a2._1), b.op(a2._2, a1._2))

    def zero: (A, B) = (a.zero, b.zero)
  }

  //EXERCISE 18: Do the same with Either. This is called a monoid coproduct
  def coproductMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[Either[A, B]] = new Monoid[Either[A, B]] {
    def op(a1: Either[A, B], a2: Either[A, B]): Either[A, B] =
      Either(
        a.op(a1.left.get, a2.left.get),
        b.op(a1.right.get, a2.right.get)
      )

    def zero: Either[A, B] = Either(a.zero, b.zero)
  }
}

trait Foldable[F[_]] { // Foldable is a higher-kinded type,
  // F[_] - the underscore indicates that D is not a type but a type constructor
  import monoid.Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(a => (b: B) => f(a, b))(EndoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(EndoMonoid[B])(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] =
    foldLeft(fa)(List[A]())((a, b) => b :: a)
}

//EXERCISE 13
trait FoldableList extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
}

trait FoldableIndexedSeq extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

trait FoldableStream extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))

}

//EXERCISE 14
trait FoldableTree extends Foldable[Tree] {

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }

  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }
}

//EXERCISE 15
trait FoldableOption extends Foldable[Option] {
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
    case None => z
    case Some(v) => f(v, z)
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
    case None => z
    case Some(v) => f(z, v)
  }

  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(v) => f(v)
  }
}