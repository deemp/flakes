package part4

import scala.annotation.tailrec
import java.{util => ju}

object Ex extends App {
  case class Logger(msgNum: Int = 0) {
    def info = {
      println("INFO New Message")
      new Logger(msgNum + 1)
    }
    @tailrec
    final def info(n: Int): Logger = if (n <= 0) this else info.info(n - 1)
    def print = println(msgNum)
  }
  val t = new Logger(3)
  // t.info
  // println("--")
  // t.info(3)
  // println("--")
  // t.print

  // allow A and its supertypes
  // covariance
  abstract class LogList[+A]() {
    // T is a supertype of A
    def add[T >: A](msg: T): LogList[T]
    def last[T >: A]: T
    def previous[T >: A]: LogList[T]
    def isEmpty: Boolean
    def all: String
  }

  class Log[+A](head: A, tail: LogList[A] = Empty) extends LogList[A] {
    def add[T >: A](msg: T): LogList[T] = new Log(msg, this)
    def last[T >: A] = head
    def previous[T >: A]: LogList[T] = tail
    def isEmpty: Boolean = false
    def all: String = {
      if (tail == Empty) s"$head"
      else
        s"$head ${tail.all}"
    }
  }

  object Empty extends LogList {
    def add[T >: Nothing](msg: T): LogList[T] = new Log(msg, Empty)
    def last[T >: Nothing]: T = throw new ju.NoSuchElementException()
    def previous[T >: Nothing]: LogList[T] = throw new ju.NoSuchElementException()
    def isEmpty: Boolean = true
    def all: String = ""
  }

  val intLogs: LogList[Int] = Empty
  val strLogs: LogList[String] = Empty

  val list = new Log("m1", new Log("m2", new Log("m3", Empty)))
  println(list.all)
}
