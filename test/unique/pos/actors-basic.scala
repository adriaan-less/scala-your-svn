/*-enable-unique*/
import scala.annotation.{transient, unique}
import scala.actors.Actor._

@transient
class Message(var data: String)

object Test {
  def main(args: Array[String]) {
    val a = actor {
      react {
        case msg: Message =>
          println("received "+msg)
      }
    }

    actor {
      //val msg: Message @unique = new Message("text")
      // 1. check that msg is unique
      val msg: Message @unique = new Message("text")
      a ! msg
    }
  }
}
