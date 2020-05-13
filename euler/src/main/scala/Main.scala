object Main extends App {

  if (args.size == 0)
    println("Specify a question number!")
  else
    println(
    args(0) match {
      case "1" => Problem1
      case _ => "Not solved yet!"
    }
  )

  def Problem1 = {
    val range = (1 to 999).toList
    val multiples = range.filter(i => i % 3 == 0 || i % 5 == 0)
    multiples.reduce(_ + _)
  }
}