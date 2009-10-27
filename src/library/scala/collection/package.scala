package scala

package object collection {
  import scala.collection.generic.{CanBuild, CanBuildFrom}
  
  def breakOut[From, T, To](implicit b : CanBuild[T, To]) =
    new CanBuildFrom[From, T, To] {
      def apply(from: From) = b.apply() ; def apply() = b.apply()
    }
}
