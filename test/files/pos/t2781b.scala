import scala.collection.{TraversableLike, IterableLike}

object Test { // TODO: reinstate variance?
  sealed abstract class ViewableAs[From, To] extends (From => To)
  object ViewableAs {
    implicit def conformsOrViewsAs[A <% B, B]: A ViewableAs B = new (A ViewableAs B) {def apply(x: A) = x}
  }

  case class MyTuple2[T1, T2](_1:T1, _2:T2) {
    def zipped[Repr1, El1, Repr2, El2]
              (implicit w1: T1 ViewableAs TraversableLike[El1, Repr1], w2: T2 ViewableAs IterableLike[El2, Repr2])
                : Zipped[Repr1, El1, Repr2, El2] = new Zipped[Repr1, El1, Repr2, El2](_1, _2)
  
    class Zipped[Repr1, El1, Repr2, El2](coll1: TraversableLike[El1, Repr1], coll2: IterableLike[El2, Repr2])
  }

  val as = Array(1, 2, 3)
  MyTuple2(as, as) zipped
}