/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
*                                                                      */

// $Id$


package scala.collection.mutable

import scala.collection.generic._

/** This class implements single linked lists where both the head (<code>elem</code>)
 *  and the tail (<code>next</code>) are mutable.
 *
 *  @author Matthias Zenger
 *  @author Martin Odersky
 *  @version 2.8
 */
@serializable
class DoubleLinkedList[A](_elem: A, _next: DoubleLinkedList[A]) extends LinearSequence[A]
                                                                   with TraversableClass[A, DoubleLinkedList]
                                                                   with DoubleLinkedListTemplate[A, DoubleLinkedList[A]] {
  elem = _elem
  next = _next
  override def companion: Companion[DoubleLinkedList] = DoubleLinkedList
}

object DoubleLinkedList extends SequenceFactory[DoubleLinkedList] {
  implicit def builderFactory[A]: BuilderFactory[A, DoubleLinkedList[A], Coll] = //new BuilderFactory[A, DoubleLinkedList[A], Coll] { def apply(from: Coll) = from.traversableBuilder[A] }
    new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, DoubleLinkedList[A]] =
    new Builder[A, DoubleLinkedList[A]] {
      var current: DoubleLinkedList[A] = _
      def +=(elem: A): this.type = {
        val tmp = new DoubleLinkedList(elem, null)
        if (current != null)
          current.insert(tmp)
        else
          current = tmp
        this
      }
      def clear() { current = null }
      def result() = current
    }
}
