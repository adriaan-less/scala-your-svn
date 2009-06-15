import scala.collection.generic._

class IterableOps[CC[+B] <: Iterable[B] with IterableTemplate[B, CC[B]], A1, A2](tuple: (CC[A1], Iterable[A2])) {
  def unzip: (CC[A1], CC[A2]) = error("foo")
}

object Test {

  implicit def tupleOfIterableWrapper[CC[+B] <: Iterable[B] with IterableTemplate[B, CC[B]], A1, A2](tuple: (CC[A1], Iterable[A2])) 
      = new IterableOps[CC, A1, A2](tuple)
    
  val t = (List(1, 2, 3), List(6, 5, 4))

  tupleOfIterableWrapper(t) unzip

  t unzip
}