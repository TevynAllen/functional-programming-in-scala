package parsing

import propertybasedtesting.{Gen, Prop}

import scala.util.matching.Regex

//[_] scala syntax for a type parameter that is itself a type constructor
trait Parsers[Parser[+ _]] { // a type constructor type argument
  self => // so inner classes may call methods of the trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  //Always succeed with the value a
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  //Recognize and return a single String
  def string(s: String): Parser[String]

  //combinator to recognise two possible string values
  def orString(s1: String, s2: String): Parser[String]

  // Choose between two parsers, first attempting p1, then p2 if p1 fails
  //choosing between two parsers, regardless of their result type
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

//  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2) // use self to explicitly disambiguate reference to the or method on the trait
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def **[A1 >: A, B >: A](p2: => Parser[B]): Parser[(A1, B)] = self.product(p, p2)
    def product[A1 >: A, B >: A](p2: => Parser[B]): Parser[(A1, B)] = self.product(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many[A]: Parser[List[A]] = self.many(p)
    def slice[A]: Parser[String] = self.slice(p)
  }

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List()) else
    map2(p, listOfN(n - 1, p))((a, b) => a :: b)

  // A Parser[Int] that recognizes zero or more 'a' characters,
  // and whose result value is the number of 'a' characters it has seen.
  // For instance, given "aa", the parser results in 2, given "" or "b123" (a string not starting with 'a'),
  // it results in 0, and so on.
  def noOfChars(p: Parser[Char]): Parser[Int]

  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List())

  // Apply the function f to the result of p, if successful
  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(p => succeed(f(p)))

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

  }

  //Return the portion of input inspected by p if successful
  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  //Sequence two parsers, running p1, then p2 and return the pair of their results if both succeed
  //running one parser followed by another if the first parser is successful
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    flatMap(p)(a => map(p2)(b => (a, b)))

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    map(product(p, p2))(f.tupled)

  def map2Flat[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p)(a => map(p2)(b => f(a, b)))

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def sensitive[A](p: Parser[A]): Parser[A] = flatMap(p){
    case x: String => ???
  }

  implicit def regex(r: Regex): Parser[String]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  // the parse error needs to tell us where the problem occurred
  case class Location(input: String, offset: Int = 0) {
    lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col: Int = input.slice(0, offset + 1).reverse.indexOf('\n')
  }

  def errorLocation(e: ParseError): Location
  def errorString(e: ParseError): String

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def errorStack(e: ParseError): List[(Location, String)]

  case class ParseError(stack: List[(Location, String)] = List(), otherFailures: List[ParseError] = List())

  //delays committing to a parse
  def attempt[A](p: Parser[A]): Parser[A]
}