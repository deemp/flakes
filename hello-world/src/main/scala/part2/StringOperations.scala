package part2

object StringOperations extends App {
  val aString: String = "Hello, world!"
  println(aString.length) // выводит 13
  println(aString.charAt(1)) // выводит e
  println(aString.substring(0, 2)) // выводит He
  println(aString.split(" ").toList) // выводит List(Hello,, world!)
  println(aString.startsWith("He")) // выводит true
  println(aString.replace("!", ".")) // выводит Hello, world.
  println(aString.toLowerCase) // выводит hello, world!
  println(aString.toUpperCase) // выводит HELLO, WORLD!
  println("abcd".reverse) // выводит dcba
  println("abcd".take(2)) // выводит ab
  println('1' +: "42" :+ '3') // выводит 1423
  println('a' +: "bc" :+ 'd') // abcd
  println("a" ++ "bc" :++ "d") // abcd

// raw string interpolation!
  val someString = "This is \n a string"
  println(raw"This is \n a string")
  println(raw"$someString")

  // string interpolation
  val name = "John"
  val surname = "Smith"

  println(s"Hello, ${name + surname}") // выводит Hello, JohnSmith

  val link = "https://stepik.org"

  println(s"The link is ${link.toUpperCase}")

  println(s"The link is \t $link")

  println(raw"The link is \t $link")

  println(link :++ "/catalog")

}
