object Main extends App {
  val multiples = (1 to 999).toList.filter(i => i % 3 == 0 || i % 5 == 0)
  val sum = multiples.reduce(_ + _)
  println(sum)
}