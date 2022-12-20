package part0

object Zoo extends App {

  case class Rational(num: Int, denom: Int) {
    def this(s: String) = this(Rational(s)._1, Rational(s)._2)
  }

  object Rational {
    def apply(s: String): (Int, Int) =
      s.split("/").map(_.trim.toInt) match {
        case Array(a, b) => (a, b)
        case _           => throw new NumberFormatException
      }
  }

  val s = new Rational("142/342")
  println(s)
}
