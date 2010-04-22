/*-enable-unique*/
import scala.annotation.{transient, unique}

@transient
class Message(var data: String)

object Test {
  def m(x: Message @unique) {}

  def main(args: Array[String]) {
    val msg: Message @unique = new Message("text")
    val d: String @ unique = msg.data
    m(msg)
  }
}
