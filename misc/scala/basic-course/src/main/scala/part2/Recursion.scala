package part2

import scala.annotation.tailrec

object Recursion extends App {

  def factorialWithTailRecursion(n: Int): Int = {
    @tailrec
    def loop(x: Int, accumulator: Int = 1): Int = {
      if (x <= 1) accumulator
      else loop(x - 1, x * accumulator)
    }
    loop(n, 1)
  }

  def repeatWord(word: String, n: Int = 1): String = {
    @tailrec
    def loop(i: Int = n, acc: String = word): String = {
      if (i <= 1) acc
      else loop(i - 1, f"$word $acc")
    }
    loop()
  }

  // println(repeatWord("hey", 2))

  def powerOfTwo(pow: Int = 0): BigInt = {
    @tailrec
    def loop(i: Int = pow, acc: BigInt = 1): BigInt = {
      if (i == 0) acc
      else loop(i - 1, 2 * acc)
    }
    loop()
  }

  // println(powerOfTwo())

  def nPrint(x: Int, n: Int, y: Int): String = {
    @tailrec
    def loop(i: Int = n, acc: Int = 0): Int = {
      if (i == 0) acc
      else loop(i - 1, acc + y)
    }
    val x_ = x + loop()
    x_.toString().map(_ => s"$x_").mkString(" ") ++ " is the result"
  }

  // println(nPrint(fArgs(0), fArgs(1), fArgs(2)))
  // print(nPrint(10, 1, 5))

  val input = "I like   Scala"

  def reorder(str: String): String = {
    str.trim.replaceAll(" +", " ").split(" ").reverse.mkString(" ")
  }

  println(reorder(input))
}
