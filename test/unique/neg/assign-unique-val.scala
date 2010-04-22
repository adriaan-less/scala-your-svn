/*-enable-unique*/
import scala.annotation.{transient, unique}

@transient
class Message(var data: String)

object O {
  val msg = new Message("text")
}

object Test extends Application {

  // this assignment should fail, since objects are shared
  // by default
  val s: Message @unique = O.msg

}
