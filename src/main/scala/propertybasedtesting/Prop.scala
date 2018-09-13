package propertybasedtesting

sealed trait Gen[A] {

  def listOf[A](a: Gen[A]): Gen[List[A]]

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]]
}