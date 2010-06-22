package scalaz

import collection.generic.CanBuildFrom

class TraversableW[Coll, A](val value: Coll)(implicit ev: Coll <:< Traversable[A])  {
  def as[CC[X] <: Traversable[X]](implicit cbf1: CanBuildFrom[Coll, A, CC[A]] = null, cbfDefault: CanBuildFrom[Nothing, A, CC[A]]): CC[A] = {
    Option(cbf1) match {
      case Some(cbf) =>
        // A direct builder exists from the source collection to the target
        cbf(value).result
      case _ =>
        // No direct builder, we have to add elements one at a time.
        val builder = cbfDefault.apply
        value.foreach(a => builder += a)
        builder.result
    }

//    def to[Result](implicit cbf: CanBuildFrom[Nothing, A, Result]): Result = value.map(identity)(collection.breakOut)
  }
}

trait TraversableWs {
  implicit def TraversableWTo[Coll, A](t: Coll)(implicit ev: Coll <:< Traversable[A]): TraversableW[Coll, A] = new TraversableW[Coll, A](t)
}

object Test extends Application {
  val i = new TraversableWs {}
  import i._
  TraversableWTo(List(1)).as[Vector]
  TraversableWTo(Map(1 -> 2)).as[Vector]

  // error: could not find implicit value for parameter ev: <:<[List[Int],Traversable[A]] List(1).as[Vector]
  List(1).as[Vector]
}