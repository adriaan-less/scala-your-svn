/*-enable-unique*/
import scala.annotation.transient
import scala.actors.Actor

@transient
class Message(var data: String)

class MyActor extends Actor {
  def act() {
    react {
      case msg: Message =>
        sender ! msg
    }
  }
}
