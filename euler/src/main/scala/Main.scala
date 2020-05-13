import scala.collection.mutable.ArrayBuffer

object Main extends App {

  if (args.size == 0)
    println("Specify a question number!")
  else
    println(
    args(0) match {
      case "1" => Problem1
      case "2" => Problem2
      case "3" => Problem3
      case "4" => Problem4
      case _ => "Not solved yet!"
    }
  )

  def Problem1 = {
    val range = (1 to 999).toList
    val multiples = range.filter(i => i % 3 == 0 || i % 5 == 0)
    multiples.reduce(_ + _)
  }

  def Problem2 = {
    val limit = 4000000
    val fibonacci = ArrayBuffer(0, 1)
    while (fibonacci.head <= limit) {
      val nextNumber = fibonacci.take(2).reduce(_ + _)
      fibonacci.prepend(nextNumber)
    }
    fibonacci.trimEnd(1)
    fibonacci.filter(_ % 2 == 0).reduce(_ + _)
  }

  def Problem3 = {
    val target = 600851475143L
    factorise(target).max
  }

  def factorise(number: Long): List[Long] = {
    val maxToTry = number / 2
    var divisor = 2L
    while (divisor < maxToTry) {
      if (number % divisor == 0) {
        return List(divisor) ++: factorise(number / divisor)
      }
      divisor += 1
    }
    List(number)
  }

  def Problem4 = {
    val threeDigitNumbers = 100 to 999
    val products = for (x <- threeDigitNumbers; y <- threeDigitNumbers) yield x * y
    products.filter(numberIsPalindrome).max
  }

  def numberIsPalindrome(number: Int) = number.toString == number.toString.reverse
}