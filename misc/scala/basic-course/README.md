# Scala Start

## VS Code keybindings

- `Ctrl + Shift + P` -> `Preferences: Open Keyboard Shortcuts` -> `metals.run-current-file` -> set `Shift + Enter`

## Notes

- a block of code returns the last expression
- if a function has a name `getSomething`, can conventionally rename it to `something`
- values calculation - [src](https://stepik.org/lesson/460611/step/5?unit=451205)
  - `def f(x: Long): Unit` -> call by value
    - value calculated before function call
      - good: calculated only once
  - `def f(x: => Long): Unit` -> call by name
    - value calculated inside function
      - good: not calculated if unused in function body

- can declare functions inside functions
  - a wrapper and a recursive subfunction

    ```scala
    def wrapperFun(_*): T = {
        def recursiveFun(_*, acc: T) = {
                // do recursive calculation
            }

        recurciveFun(_*, initialAccValue)
    }
    ```

- tail recursion - accumulate results in a function's parameter to avoid stack overflow
  - function design - [src](https://stepik.org/lesson/460612/step/6?unit=451206)
  - can set default accumulator value
    - `def loop(x: Int, accumulator: Int = 1)`
  - can verify tail recursiveness

    ```scala
    import scala.annotation.tailrec
    // ...
    @tailrec
    def loop(x: Int, accumulator: Int = 1): Int = {
    ```

- run with reload on changes - [src](https://stackoverflow.com/a/26144534)
  - `~runMain week1.StringOperations`

- set imports

  ```scala
  import scala.util.{Try, Failure, Success}
  ```

- can import anywhere

## week 3

- use `val` to make a class constructor argument available from the outside - [src](https://stepik.org/lesson/460613/step/3?unit=451207)

  ```scala
  class SomeClass(a: Int, b: Int, val c: Int) {
    def someFunc(): Int = b
  }
  ```

- Scala generated getters and setters automatically - [src](https://stepik.org/lesson/460613/step/5?discussion=5600530&unit=451207)
  - can override them
- class body is just a block
- can require some properties

  ```scala
  class A(a: Int) {
    require(a > 3)
  }
  val b = Try(new A(2))
  println(b)
  ```

- can access anything defined in the class body - [src](https://stepik.org/lesson/460613/step/5?unit=451207)
- Actions inside class body are executed on instantiation - same src

- use `this.parameter` to refer to a class parameter, not to a class method parameter - [src](https://stepik.org/lesson/460613/step/8?unit=451207)

- method overloading is possible if two class methods have different signatures and or types - [src](https://stepik.org/lesson/460613/step/9?unit=451207)
  - `getId: String` and `getId(name: String): String`

- can overload constructors via `def this` - [src](https://stepik.org/lesson/460613/step/10?unit=451207)

## Objects

- create objects to store data - like namespace - [comment](https://stepik.org/lesson/463103/step/1?discussion=4003749&reply=4245066&unit=453728)

- Objects are `Singleton objects`, meaning that there exists a single object
  - values of vals storing the same object will be equal: `a == b` - [src]([src](https://stepik.org/lesson/463103/step/3?unit=453728))
- Should not compare instances of a class like `a == b`
  - This will be `false`
- Actions inside object body are executed only **on declaration**, because it's also the definition

  ```scala
    object A {
      val a: String = "value a"
      println("object A")
    }

    val aVal = A
    val anotherVal = A

    println(aVal.a)
    println(anotherVal.a)
  ```

### Companions

- Object and class with the same name defined in a single file - [src](https://stepik.org/lesson/463103/step/4?unit=453728)
  - companion object
- can create `apply` in a companion object to use more complex constructors - [src](https://stepik.org/lesson/463103/step/5?unit=453728)

### Inheritance

- a child class `extends` a parent class - [src](https://stepik.org/lesson/463104/step/1?unit=453729)
- class field modifiers: - [src](https://www.jesperdj.com/2016/01/08/scala-access-modifiers-and-qualifiers-in-detail/)
  - private - available to an companion object, but not to child classes
  - protected - available to a child class, not available from outside
  - no modifier - available from outside
- override in a constructor: - [src](https://stepik.org/lesson/463104/step/7?unit=453729)
  - `class A(override val b: String)`
- can use `super`
- polymorphism - [src](https://stepik.org/lesson/463104/step/9?unit=453729)
- prohibit overriding - [src](https://stepik.org/lesson/463104/step/11?unit=453729)
  - `final class_member` - prohibit `override` `class_member`
  - `final class` - prohibit class extension
  - `sealed` - allow class extensions in current file, but not in other files
- abstract class - abstract fields have no bodies - [src](https://stepik.org/lesson/463104/step/12?unit=453729)
  - definitions in child classes
- anonymous class - for single-time usage - [src](https://stepik.org/lesson/463104/step/13?unit=453729)
- `trait` - like typeclasses, but have no type parameters - [src](https://stepik.org/lesson/463104/step/15?unit=453729)
  - can set multiple traits

    ```scala
    class A () extends B with Trait1 with Trait2 {}
    ```

### Exceptions

- Nothing is at the bottom of the scala type system - [src](https://json.schemastore.org/github-workflow.json)
  - Nothing is anything - [src](https://stepik.org/lesson/463107/step/4?discussion=3751279&reply=3909920&unit=453732)

- need to accommodate the type of try and catch - choose the least general

  ```scala
    val potentialException = try {
    intOrNothing(false)
  } catch {
    case e: RuntimeException => println("RTE is here")
  } finally {
    println("I will be there no matter what")
  }
  ```

- throw exceptions
  - `def unsafeMethod(): String = throw new RuntimeException("Sorry, not your day")`

- `Int.MaxValue`  is the largest number - [src](https://stepik.org/lesson/463107/step/6?unit=453732)
  - any number larger than it will have a negative sign

### Generics

- type parameters start with an uppercase letter - [src](https://stepik.org/lesson/771289/step/3?unit=773730)

  ```scala
  class SomeClass[T] {
    def someFunc(aVal: T): Unit = println(s"generic type is used")
  }
  ```

  ```scala
  def randomItem[A](items: List[A]): A = {
    val randomIndex = util.Random.nextInt(items.length)
    items(randomIndex)
  }

  ```

- Generics are for classes, traits, methods, not for objects

- Upper and lower type bounds: - [src](https://stepik.org/lesson/771289/step/5?unit=773730)
  - `T <: Vehicle` - `T` is a subtype of `Vehicle`
  - `T >: Vehicle` - `T` is a supertype of `Vehicle`
  - combined:  `class Parking[T >: Bicycle <: Vehicle](val vehicle: T)`

### Variance

- Variance problem: if `A` `extend`s `B`, will `List[A]` `extend` `List[B]`?
  - `+А` - instance will be a supertype of a type parameter
  - `-А` - instance will be a supertype of a type parameter
- like `Foo[+A]` - [src](https://docs.scala-lang.org/tour/variances.html)
  - `A <: B` - `A` is a subtype of `B`  
  - `A <: B`, class `Cov[+T]` => `Cov[A] <: Cov[B]`
  - `A >: B`, class `Contra[-T]` => `Contra[B] >: Contra[A]`
- an invariant type parameter requires usage of this parameter

### Syntactic sugar

- notations - better use `.` - [src](https://stepik.org/lesson/464328/step/3?unit=454974)
  - infix notation: `println(bob worksAs "Developer")` - if a method takes only one parameter
  - postfix notation `println(bob speaksEnglish)` - if a method doesn't take any parameters
- operators like `+`: - [src](https://stepik.org/lesson/464328/step/4?unit=454974)
  - are methods accessible via:
    - `.`:  `1.+(2)`
    - infix notation: `bob & alice`
  - can be overriden
- there are unary operators - [src](https://stepik.org/lesson/464328/step/6?unit=454974)

### Case classes

- avoid ordinary classes - [src](https://stepik.org/lesson/463106/step/1?unit=453731)
  - case class features - [src](https://stepik.org/lesson/463106/step/1?discussion=3913327&reply=3916500&unit=453731)
    - immutable
    - elementwise comparison
    - aVal = Aval anotherVal
    - can be copied
    - can be pretty printed
    - its parameters are its fields by default - [src](https://stepik.org/lesson/463106/step/2?unit=453731)
    - has content level equality property - [src](https://stepik.org/lesson/463106/step/4?unit=453731)
      - instance `A` can be compared to instance `B`
    - has a `copy` method - [src](https://stepik.org/lesson/463106/step/5?unit=453731)
      - `val bobsTwin = bob.copy(name = "John")`
    - has a companion object
      - `val alice = Person("Alice", "Engineer") // метод apply в действии`
    -
- put functions and values into traits and objects

## Functional Programming

1. There are traits for functions in Scala

  ```scala
  val res = new Function1[Int, Int] {
    override def apply(x: Int): Int = x * 2
  }
  
  ```

- last parameter is the type of the return value
- there are `Function1 .. Function22`

![lambda](./README/3.lambda.png)

1. Examples

   - 3 is a bit Haskell-y
   - `val strlen: String => Int = s => s.length()`

1. Currying

  ```scala
  def add(x: Int)(y: Int) = x + y

  println(add(1)(2))
  ```

- usecases - [src](https://stepik.org/lesson/464402/step/3?discussion=5174427&reply=5203868&unit=455048)
  - reduce the number of data traversals
  - make a good alias of a partially applied function

### ADT

- Sum of Products - use `case class`es

  ```scala
  sealed trait Platform
  case class IOS(appId: String) extends Platform
  case class Android(packageId: String, sha1Cert: String) extends Platform
  ```

- Sum - use `case object`s

  ```scala
  sealed trait WeekDay
  case object Mon extends WeekDay
  case object Tue extends WeekDay

  val day: WeekDay = Mon
  println(s"Today is $day") // Today is Mon
  ```

More examples - [src](https://stepik.org/lesson/822904/step/5?unit=826340)

### Collections

- `Set`, `Seq`, `Map` - [src](https://stepik.org/lesson/466069/step/2?unit=456826)
- `Array`
  - `array(5) = '!'`
- `Tuple`
  - `(2,3)` or `Tuple2(2, 3)`
- `Seq`
  - `1 until 3` - 1, 2
  - `(1 to 3).foreach(x => print("Hello"))`
- `Vector`
  - `(1 to 5).toVector` - `Seq` to `Vector`
- `flatMap` - `concatMap` - map and concat results

  ```scala
  val combinations = list1.flatMap(n => list2.map(c => c + n))
  ```

- list comprehension

  ```scala
  val forCombination = for {
    n <- list1 if n > 1
    c <- list2
  } yield c + n
  ```

- In Scala 3 - [src](https://docs.scala-lang.org/scala3/book/taste-control-structures.html#guards)

  ```scala
  for
    i <- 1 to 3
    j <- 'a' to 'c'
    if i == 2
    if j == 'b'
  do
    println(s"i = $i, j = $j")
  ```

- `Array.ofDim[Boolean](1).forEach(println)` - Creates array with given dimensions - [src](https://www.scala-lang.org/api/2.12.4/scala/Array$.html#ofDim[T](n1:Int)(implicitevidence$3:scala.reflect.ClassTag[T]):Array[T])

- `String + Any` or `Any + String` are implemented - [src](https://stackoverflow.com/a/44147224)
  - "3" + 5

- `Option` checks for `null` - [src](https://stepik.org/lesson/466070/step/1?unit=456827)
  - `Option(null) == None`
  - `Option(3) == Some(3)`

- use as a return type
  - `def foo(): Option[String] = None`

- can chain

  ```scala
  val chainedResult = Option(unsafeMethod()).orElse(Option(maybeSafeMethod()))    
  ```

- if we don't trust an API, can wrap into `Option` - [src](https://stepik.org/lesson/466070/step/3?unit=456827)
  - our API should ship with `Option`

- use `isEmpty` to test if an `Option` contains anything

- `Try` - wrap values with exceptions - [src](https://stepik.org/lesson/466071/step/2?unit=456828)
  - use instead of `try-catch-finally`

  ```scala
  val potentialFailure = Try(unsafeMethod())
  println(potentialFailure)
  ```

  - chain

  ```scala
  Try(unsafeMethod()).orElse(Try(myMethod()))
  methodWhichFails() orElse methodWhichSucceeds()
  ```

- Pattern matching

  ```scala
  val description = someVal match {
    case 1 => "action 1"
    case 2 => "action 2"
    _ => "action3"
  }
  ```

  - `_` denotes the default case

- named pattern (like as-pattern in Haskell) - [src](https://stepik.org/lesson/466073/step/3?unit=456830)

  ```scala
  case nonEmptyList@List(1, _, _, _) => s"нашли $nonEmptyList"
  case someList@List(6, _*) => s"нашли список $someList"
  ```

  - `_` - some element
  - `_*` - some elements

- scala matches from top to bottom without generics - [src](https://stepik.org/lesson/466073/step/4?unit=456830)
  - this outputs `a list of strings`

  ```scala
  val numbers = List(1, 2, 3)
  val numbersMatch = numbers match {
    case listOfStrings: List[String] if condition => "a list of strings"
    case listOfNumbers: List[Int] => "a list of integers"
    case _ => "default case"
  }
  ```

- Can use partial functions and handle `MatchError` - [src](https://stepik.org/lesson/470156/step/1?unit=461011)

  ```scala
   val aPartialFunction: PartialFunction[String, String] = {
    case "mon" => "Work!"
    case "fri" => "Party Time"
    case "sun" => "Relax a little"
  } 
  ```

- can check if an argument is usable

  ```scala
  aPartialFunction.isDefinedAt("tue")
  ```

- can chain

  ```scala
  aPartialFunction.orElse[String, String]{...}
  ```

- can `lift` and get `Option`

  ```scala
  aPartialFunction.lift
  ```

- `unapply` - for pattern matching, unapply a constructor - [src](https://docs.scala-lang.org/tour/extractor-objects.html)

  ```scala
  import scala.util.Random

  object CustomerID:

    def apply(name: String) = s"$name--${Random.nextLong()}"

    def unapply(customerID: String): Option[String] =
      val stringArray: Array[String] = customerID.split("--")
      if stringArray.tail.nonEmpty then Some(stringArray.head) else None

  val customer1ID = CustomerID("Sukyoung")  // Sukyoung--23098234908
  customer1ID match
    case CustomerID(name) => println(name)  // prints Sukyoung
    case _ => println("Could not extract a CustomerID")
  ```

## FP Practice

- use `foreach` for things returning unit values
- update `Map` - [src](https://stepik.org/lesson/469642/step/6?unit=460466)

  ```scala
  network + (pointA -> (routesForA + pointB)) + (pointB -> (routesForB + pointA))
  ```

- count empty values

  ```scala
  network.count(pair => pair._2.isEmpty)
  network.view.filterKeys(key => network(key).size == 0).size
  network.view.filterKeys(key => network(key).isEmpty).size
  network.view.filter(pair => pair._2.isEmpty).size
  network.count(_._2.isEmpty) 
  ```

## Scala 3

- [enum](https://docs.scala-lang.org/scala3/reference/enums/enums.html)
  - creates companion objects
- use `val` with HOF - better performance due to immutability

## Design

- Service/Handle pattern - [src](https://www.metalamp.ru/articles/service-handle-pattern)
  - access a `service`, not a concrete function
    - hide concrete implementation behind the handle so that it's easier to substitute the concrete implementation

## Code

- Scala code organization - [src](https://blog.softwaremill.com/how-to-structure-your-scala-application-658168fbb827)
