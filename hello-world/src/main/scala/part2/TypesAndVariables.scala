package part2

object TypesAndVariables extends App {
  // val - immutable
  // var - mutable
  val a = "huh"
  println("Hello, World!")
  val aNumber =
    if (("string".length == 6 & 1 < 2) & ('1' +: "23" :+ '4').toInt == 1234) 24
    else 123
  val block = {
    val a = 2
    val b = 3
    a + b
  }
}
