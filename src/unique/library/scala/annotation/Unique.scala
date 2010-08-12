package scala.annotation

class unique extends StaticAnnotation

class transient extends StaticAnnotation

class peer(x: AnyRef) extends StaticAnnotation

class uncheckedUnique extends Annotation

class immutable extends Annotation

class safe extends StaticAnnotation

class assignable extends Annotation

object UniqueOps {
  def swap[A, B <: A](to: A, from: B): A = to
  def capture[A, B](from: A, to: B): A = from
  def share[A](x: A): A = x
  def share2[A, B](x: A, y: B) {}
}
