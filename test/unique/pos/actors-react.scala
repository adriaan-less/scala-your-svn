/*-enable-unique*/
import scala.annotation.{transient, unique}
import scala.actors.{Actor, TIMEOUT}

@transient
class Message(var data: String)

class MyActor extends Actor {
  def m(x: Message @unique) {}

  def act() {
    react {
      case msg: Message =>
        m(msg)
        reactWithin(500) {
          case TIMEOUT =>
          case msg2: Message =>
            m(msg2)
        }
    }
  }
}
