package part5

object Main extends App {
  val strlen: String => Int = s => s.length()

  def someFunc: Int => Function1[Int, Int] =
    new Function1[Int, Function1[Int, Int]] {
      override def apply(x: Int): Function1[Int, Int] =
        new Function1[Int, Int] {
          override def apply(y: Int): Int = x + y
        }
    }
  def someFuncA: Int => Int => Int = x => y => x + y
  val res = someFunc(1)
  println(res)
  println(res(4))

  (1 to 3).foreach(x => print("Hello"))

  val list1 = 1 to 3
  val list2 = 1 to 6

  val forCombination = for {
    n <- list1 if n > 1
    c <- list2
  } yield c + n

  println(forCombination)

  val progLanguages = List("java", "scala", "python")
  val lngAbbrev = List("JA", "SCA", "PY")

  val combinations = for {
    abbr <- lngAbbrev
    lang <- progLanguages
    if lang.toLowerCase().startsWith(abbr.toLowerCase())
  } yield (abbr, lang)

  print(combinations)

  val sampleTuple = new Tuple2(2, "Hello, World")
  println(sampleTuple.copy(_2 = "Scala").swap._1 + 5)

  println(Option(null) == None)
  println(Option(3) == Some(3))

  import scala.util.{Try, Failure, Success}

  def methodWhichFails(): Try[String] = Failure(
    new RuntimeException("Ooops...")
  )
  def methodWhichSucceeds(): Try[String] = Success("Everything is OK")

  val tryMethods = methodWhichFails() orElse methodWhichSucceeds()

  class A(a: Int) {
    require(a > 3)
  }
  val b = Try(new A(2))
  println(b)

  val someVal = 3

  val description = someVal match {
    case 1 => "action 1"
    case 2 => "action 2"
    case 3 => "action three"
    case _ => "default action"
  }

  println(description) // action three

  def guard[A]: (A, Int) => String = (data, maxlLength) => data match {
    case data: List[A] if data.length <= maxlLength => "Задан список List допустимой длины"
    case data: List[A] if data.length > maxlLength => "Длина списка больше максимально допустимого значения"
    case data: String if data.length <= maxlLength => "Длина строки не превышает максимально допустимого значения"
    case data: String if data.length > maxlLength => "Получена строка недопустимой длины"
    case _ => "Что это? Это не строка и не список"
  }

  println(guard(List("Hello"), 5))

}
