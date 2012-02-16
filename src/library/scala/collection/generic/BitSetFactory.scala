/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package generic

import scala.collection._
import mutable.Builder

/** A template for companion objects of <code>BitSet</code>.
 *
 *  @since 2.8
 */
trait BitSetFactory[Coll <: BitSet with BitSetLike[Coll]] {
  def empty: Coll
  def newBuilder: Builder[Int, Coll]
  def apply(elems: Int*): Coll = (empty /: elems) (_ + _)
  def bitsetCanBuildFrom = new CanBuildFrom[Coll, Int, Coll] {
    def apply(from: Coll) = newBuilder
    def apply() = newBuilder
  }
}

