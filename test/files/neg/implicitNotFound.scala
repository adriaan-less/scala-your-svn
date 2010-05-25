package scala

object Test {
  List(1,2,3).map[Int, List[String]](x => 1)
}

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "Cannot construct a collection of type ${To} with elements of type ${Elem} based on a collection of type ${To}.")
trait Meh[-From, +To]
