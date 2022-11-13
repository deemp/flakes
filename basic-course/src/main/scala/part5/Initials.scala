package part5

import scala.io.StdIn.readLine
import scala.util.Try
import scala.util.Success
import scala.util.Failure
object Initials extends App {
    val err = "Oops, something is wrong"
    val res = Try(readLine().split(' ').map(x => s"${x.take(1)}.")) match {
        case Success(s) => s match {
            case Array(a, b) => s"$a $b"
            case _ => err
        }
        case _ => err
    }
    println(res)
}
