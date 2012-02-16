/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

// generated by genprod on Wed Jun 17 14:10:05 PDT 2009

package scala

import scala.collection.{TraversableLike, IterableLike}
import scala.collection.generic.CanBuildFrom


/** Tuple3 is the canonical representation of a @see Product3
 *
 */
case class Tuple3[+T1, +T2, +T3](_1: T1, _2: T2, _3: T3)
  extends Product3[T1, T2, T3]
{
  override def toString() = "(" + _1 + "," + _2 + "," + _3 + ")"

  def zip[Repr1, El1, El2, El3, To](implicit w1:   T1 => TraversableLike[El1, Repr1],
                                             w2:   T2 => Iterable[El2],
                                             w3:   T3 => Iterable[El3],
                                             cbf1: CanBuildFrom[Repr1, (El1, El2, El3), To]): To = {
    val coll1: TraversableLike[El1, Repr1] = _1
    val coll2: Iterable[El2] = _2
    val coll3: Iterable[El3] = _3
    val b1 = cbf1(coll1.repr)
    val elems2 = coll2.iterator
    val elems3 = coll3.iterator

    for(el1 <- coll1)
      if(elems2.hasNext && elems3.hasNext)
        b1 += ((el1, elems2.next, elems3.next))

    b1.result
  }

  def zipped[Repr1, El1, Repr2, El2, Repr3, El3](implicit w1: T1 => TraversableLike[El1, Repr1],
                                                          w2: T2 => IterableLike[El2, Repr2],
                                                          w3: T3 => IterableLike[El3, Repr3]): Zipped[Repr1, El1, Repr2, El2, Repr3, El3]
    = new Zipped[Repr1, El1, Repr2, El2, Repr3, El3](_1, _2, _3)

  class Zipped[+Repr1, +El1, +Repr2, +El2, +Repr3, +El3](coll1: TraversableLike[El1, Repr1],
                                                         coll2: IterableLike[El2, Repr2],
                                                         coll3: IterableLike[El3, Repr3]) {
    def map[B, To](f: (El1, El2, El3) => B)(implicit cbf: CanBuildFrom[Repr1, B, To]): To = {
     val b = cbf(coll1.repr)
     val elems2 = coll2.iterator
     val elems3 = coll3.iterator

     for(el1 <- coll1)
       if(elems2.hasNext && elems3.hasNext)
         b += f(el1, elems2.next, elems3.next)

     b.result
    }

    def flatMap[B, To](f: (El1, El2, El3) => Traversable[B])(implicit cbf: CanBuildFrom[Repr1, B, To]): To = {
      val b = cbf(coll1.repr)
      val elems2 = coll2.iterator
      val elems3 = coll3.iterator

      for (el1 <- coll1) {
        if (elems2.hasNext && elems3.hasNext)
          b += f(el1, elems2.next, elems3.next)
        else
          return b.result
      }
      b.result
    }

    def flatMap[B, To](f: (El1, El2, El3) => TraversableOnce[B])(implicit cbf: CBF[Repr1, B, To]): To = {
      val b = cbf(coll1.repr)
      val elems2 = coll2.iterator
      val elems3 = coll3.iterator

      for (el1 <- coll1) {
        if (elems2.hasNext && elems3.hasNext)
          b ++= f(el1, elems2.next, elems3.next)
        else
          return b.result
      }
      b.result
    }

    def filter[To1, To2, To3](f: (El1, El2, El3) => Boolean)(
                 implicit cbf1: CanBuildFrom[Repr1, El1, To1],
                          cbf2: CanBuildFrom[Repr2, El2, To2],
                          cbf3: CanBuildFrom[Repr3, El3, To3]): (To1, To2, To3) = {
      val b1 = cbf1(coll1.repr)
      val b2 = cbf2(coll2.repr)
      val b3 = cbf3(coll3.repr)
      val elems2 = coll2.iterator
      val elems3 = coll3.iterator

      for(el1 <- coll1) {
        if(elems2.hasNext && elems3.hasNext) {
          val el2 = elems2.next
          val el3 = elems3.next

          if (f(el1, el2, el3)) {
            b1 += el1
            b2 += el2
            b3 += el3
          }
        }
        else return result
      }

      (b1.result, b2.result, b3.result)
    }

    def exists(f: (El1, El2, El3) => Boolean): Boolean = {
      val elems2 = coll2.iterator
      val elems3 = coll3.iterator

      for (el1 <- coll1) {
        if (elems2.hasNext && elems3.hasNext) {
          if (f(el1, elems2.next, elems3.next))
            return true
        }
        else return false
      }
      false
    }

    def forall(f: (El1, El2, El3) => Boolean): Boolean = {
      var acc = true
      val elems2 = coll2.iterator
      val elems3 = coll3.iterator

    def forall(f: (El1, El2, El3) => Boolean): Boolean =
      !exists((x, y, z) => !f(x, y, z))

      acc
    }

    def foreach[U](f: (El1, El2, El3) => U): Unit = {
      val elems2 = coll2.iterator
      val elems3 = coll3.iterator

      for (el1 <- coll1) {
        if (elems2.hasNext && elems3.hasNext)
          f(el1, elems2.next, elems3.next)
        else
          return
      }
    }
  }
}
