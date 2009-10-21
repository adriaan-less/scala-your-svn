/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package generic

import mutable.Builder

/** A template for companion objects of <code>mutable.Map</code> and
 *  subclasses thereof.
 *
 *  @since 2.8
 */
abstract class MapFactory[CC[A, B] <: Map[A, B] with MapLike[A, B, CC[A, B]]] {

  type Coll = CC[_, _]

  def newBuilder[A, B]: Builder[(A, B), CC[A, B]]

  def empty[A, B]: CC[A, B]

  def apply[A, B](elems: (A, B)*): CC[A, B] = (newBuilder[A, B] ++= elems).result

  class MapCanBuildFrom[A, B] extends CanBuildFrom[(A, B), CC[A, B], Coll] {
    : Coll) = newBuilder[A, B]
    def apply() = newBuilder
  }
}
