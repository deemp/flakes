package part2.part6

import java.io.IOException
import scala.util.Try

object Main extends App {
  object HttpService {
    @throws[IOException]("Someone else took the port")
    def apply(host: String, port: String): Connection = new Connection()
  }

  class Connection {
    @throws[IOException]("Your Connection was Interrupted")
    def get(url: String): String = ""
  }

  def render(s: String) = print(s)

  val host = ""
  val port = ""
  val url = ""

  for {
    conn <- Try(HttpService(host, port))
    page <- Try(conn.get(""))
  } yield render(page)

  val network: Map[String, Set[String]] = List().toMap
  val res = network + ("o" -> Set())
  // network
}
