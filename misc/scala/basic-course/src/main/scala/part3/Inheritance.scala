package `3-OOP`

object Inheritance extends App {
  class Button(label: String) {
    def click(): String = s"button -$label- has been clicked"
    def this() = this("test")
  }
  class RoundedButton(label: String) extends Button(label) {
    override def click(): String = s"rounded ${super.click}"
  }
  abstract class Event {
    def trigger(eventName: String): Unit
  }

  class Listener(val eventName: String, var event: Event) {
    def register(evt: Event): Unit = { event = evt }
    def sendNotification(): Unit = { event.trigger(eventName) }
  }

  val notification: Listener = new Listener("mousedown", null)
  notification.register(
    new Event {
      override def trigger(eventName: String) =
        print(s"trigger $eventName event")
    }
  )
}
