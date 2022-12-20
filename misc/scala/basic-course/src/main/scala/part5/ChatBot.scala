package part5

object ChatBot extends App {

  val chatbot: String => Option[String] = {
    val chatbot_ : PartialFunction[String, String] = {
      case "hello"     => "Hi, I'm Bot"
      case "bye"       => "Bye-bye"
      case "what's up" => "sup-sup"
    }
    chatbot_.lift
  }

//   val p = chatbot("bye")
//   println(p)
  val inp = List("hello", "bye")
//   inp.foreach(line => println(chatbot(line)))
//   inp.map(chatbot).foreach(println)

  val number = "9-876-543-21-09"
  val ans: Map[Char, Int] = number.groupMapReduce(identity)(_ => 1)(_ + _) - '-'
//   print(ans)

  val tests: List[(String, String, Int)] = List(
    ("1.0.2.03", "1.1.0", -1),
    ("2.1", "2.01", 0),
    ("3.0", "3.0.0.0", 0),
    ("4", "4.0.0.1", -1),
    ("4.0.1", "4.0.0.1", 1)
  )
  def compare(v1: String, v2: String): Int = {
    val n = (s: String) => s.replaceAll("\\.0+", ".").replaceAll("\\.+$", "")
    n(v1).compareTo(n(v2)).signum
  }

  sealed class Connection(val host: String, val port: String) {
    val connect: Option[String] = Some("dk")
  }
  object Connection {
    def apply(host: String, port: String) = Option(new Connection(host, port))
  }
  val config: Map[String, String] = List(("host", "234"), ("port", "23")).toMap
  config
    .get("host")
    .flatMap(host =>
      config
        .get("port")
        .flatMap(port => Connection(host, port))
        .map(connection => connection.connect)
    )
    .foreach(println)

  for {
    host <- config.get("host")
    port <- config.get("port")
    connection <- Connection(host, port)
  } println(connection.connect)
//   println("1.0.2.03".split("\\.").mkString("|"))
//   println(tests.map({case (a, b, c) => compare(a, b) == c}))
//   println(tests.map({ case (a, b, c) => compare(a, b) == c }))
//   println(tests.forall({ case (a, b, c) => (compare(a, b) == c): Boolean }))
}
