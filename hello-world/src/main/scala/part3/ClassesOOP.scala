package part3

object ClassesOOP extends App {
  class Student(val id: Int, val name: String) {}
  val student = new Student(3, "Bob")
//   println(student.name)

  class Instructor(val id: Int, name: String, surname: String) {
    def normalize(str: String): String = {
      val t = str.splitAt(1)
      s"${t._1.toUpperCase()}${t._2}"
    }
    def fullName() = s"${normalize(name)} ${normalize(surname)}"
  }

//   println((new Instructor(1, "bs", "do")).fullName())

  class Course(
      courseID: Int,
      title: String,
      val releaseYear: String,
      val instructor: Instructor
  ) {
    def getID(): String = s"$courseID${instructor.id}"
    def isTaughtBy(instructor: Instructor): Boolean =
      instructor == this.instructor
    def copyCourse(newReleaseYear: String): Course =
      new Course(courseID, title, newReleaseYear, instructor)
  }

  object ClassesTask {
    def main(): Unit = {
      val instructor: Instructor = new Instructor(1, "вася", "пупкин")
      println(instructor.fullName()) // Вася Пупкин

      val course: Course = new Course(2, "Course Name", "1234", instructor)
      println(course.instructor.fullName()) // Вася Пупкин
      println(course.getID) // 21
      println(course.isTaughtBy(instructor)) // true
      println(course.isTaughtBy(new Instructor(1, "Вася", "Пупкин"))) // false

      println(course.copyCourse("4321").releaseYear) // 4321
    }
  }

//   ClassesTask.main()

  class Person(name: String)

  object Person {
    val age = 30
  }

  val bob = new Person("Bob")
  println(Person.age)

  // task
  object Configs {
    val ACCOUNT_TYPE_DEFAULT = "free"
    val ACCOUNT_TYPE_PAID = "paid"
    val SUBSCRIPTION_STATUS = "active"
  }

  object Settings {
    case class AccountSettings(email: String, password: String, picture: String)

    case class SubscriptionSettings(
        payment: String,
        notifications: String,
        expiration: String
    )
  }

  trait Subscriber {
    def subscribe(settings: Settings.SubscriptionSettings): Unit = println(
      "subscribed"
    )
  }

  trait Unsubscriber {
    def unsubscribe(): Unit = println("unsubscribed")
  }

  abstract class Account(
      accountID: String,
      settings: Settings.AccountSettings
  ) {
    def info(): Unit
  }

  class FreeAccount(accountID: String, settings: Settings.AccountSettings)
      extends Account(accountID, settings)
      with Subscriber {

    override def info(): Unit = println(
      s"Account Type: ${Configs.ACCOUNT_TYPE_DEFAULT}"
    )
  }

  class PaidAccount(accountID: String, settings: Settings.AccountSettings)
      extends Account(accountID, settings)
      with Unsubscriber {

    override def info(): Unit = {
      println(s"Account Type: ${Configs.ACCOUNT_TYPE_PAID}")
      println(s"Subscription Status: ${Configs.SUBSCRIPTION_STATUS}")
    }
  }
}
