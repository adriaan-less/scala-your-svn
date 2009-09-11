/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: WrappedFloatArray.scala 18572 2009-08-25 14:14:11Z odersky $


package scala.collection.mutable
import scala.reflect.ClassManifest

@serializable
final class WrappedFloatArray(val array: Array[Float]) extends WrappedArray[Float] {

  def elemManifest = ClassManifest.Float

  def length: Int = array.length

  def apply(index: Int): Float = array(index)

  def update(index: Int, elem: Float) {
    array(index) = elem
  }
  def unbox(elemClass: Class[_]): AnyRef = array
}
