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
      case "7" => Problem7
      case "8" => Problem8
      case "9" => Problem9
      case "10" => Problem10
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
    var count = 1
    var numberToTest = 1
    while (count < 10001) {
      numberToTest += 2
      if (isPrime(numberToTest)) {
        count += 1
      }
    }
    numberToTest
  }

  def isPrime(number: Int): Boolean = {
    if (number % 2 == 0 && number != 2) return false
    for (i <- 3 to number / 2 by 2) {
      if (number % i == 0) return false
    }
    true
  }

  def Problem8 = {
    val thousandDigitNumber = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
    val digits = thousandDigitNumber.map(_.asDigit.toLong)
    digits.iterator.sliding(13).map(_.product).max
  }

  def Problem9 = {
    for {
      c <- 1 to 1000; b <- 1 to c; a <- 1 to b
      if pow(a, 2) + pow(b, 2) == pow(c, 2) && a + b + c == 1000
    } yield a * b * c
  }

  def Problem10 = {
    println("This may take a while, go make a cup of tea...")
    val numbers = 2 to 1999999
    numbers.filter(isPrime(_)).map(_.toLong).sum
  }

}