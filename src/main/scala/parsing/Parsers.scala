package parsing

//[_] scala syntax for a type parameter that is itself a type constructor
trait Parsers[ParseError, Parser[+_]] { // a type constructor type argument

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char]
  //combinator to recognise two possible string values
  def orString(s1: String, s2: String): Parser[String]

  //choosing between two parsers, regardless of their result type
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2) // use self to explicitly disambiguate reference to the or method on the trait
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
  }

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  // A Parser[Int] that recognizes zero or more 'a' characters,
  // and whose result value is the number of 'a' characters it has seen.
  // For instance, given "aa", the parser results in 2, given "" or "b123" (a string not starting with 'a'),
  // it results in 0, and so on.
  def noOfChars(p: Parser[Char]): Parser[Int]
  def many[A](p: Parser[A]): Parser[List[A]]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]



}