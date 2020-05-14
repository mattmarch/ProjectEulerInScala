import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.math.pow
import scala.math.BigInt

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
      case "5" => Problem5
      case "6" => Problem6
      case _ => "Not solved yet!"
    }
  )

  def Problem1 = {
    val range = (1 to 999).toList
    val multiples = range.filter(i => i % 3 == 0 || i % 5 == 0)
    multiples.sum
  }

  def Problem2 = {
    val limit = 4000000
    val fibonacci = ArrayBuffer(0, 1)
    while (fibonacci.head <= limit) {
      val nextNumber = fibonacci.take(2).sum
      fibonacci.prepend(nextNumber)
    }
    fibonacci.trimEnd(1)
    fibonacci.filter(_ % 2 == 0).sum
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

  def Problem5 = {
    val factorCounts = Map[Int, Int]()
    for (i <- 2 to 20) {
      val factors = factoriseInt(i)
      for (factor <- factors.distinct) {
        val factorCount = factors.count(_ == factor)
        if (!factorCounts.contains(factor) || factorCount > factorCounts(factor)) {
          factorCounts += (factor -> factorCount)
        }
      }
    }
    factorCounts.keys.reduce((total, factor) => total * pow(factor, factorCounts(factor)).toInt)

  }

  def factoriseInt(number: Int): List[Int] = {
      factorise(number.toLong).map(_.toInt)
  }

  def Problem6 = {
    val natNumbers = 1 to 100
    val squaredSum = BigInt(natNumbers.sum).pow(2)
    val sumSquares = natNumbers.map(BigInt(_)pow(2)).sum
    squaredSum - sumSquares
  }

  def Problem7 = {

  }

}