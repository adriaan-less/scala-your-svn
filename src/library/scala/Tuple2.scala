/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala

import annotation.unchecked.uncheckedVariance
import scala.collection.Traversable
import scala.collection.generic.TraversableClass

object Tuple2 {
/* !!! todo: enable
  class IterableOps[CC[+B] <: Iterable[B] with IterableTemplate[CC, B @uncheckedVariance], A1, A2](tuple: (CC[A1], Iterable[A2])) {
    def zip: CC[(A1, A2)] = {
      val elems1 = tuple._1.iterator
      val elems2 = tuple._2.iterator
      val b = (tuple._1: IterableTemplate[CC, A1]).newBuilder[(A1, A2)]
        // : needed because otherwise it picks Iterable's builder.
      while (elems1.hasNext && elems2.hasNext)
        b += ((elems1.next, elems2.next))
      b.result
    }
    def map[B](f: (A1, A2) => B): CC[B] = {
      val elems1 = tuple._1.iterator
      val elems2 = tuple._2.iterator
      val b = (tuple._1: IterableTemplate[CC, A1]).newBuilder[B]
      while (elems1.hasNext && elems2.hasNext)
        b += f(elems1.next, elems2.next)
      b.result
    }
    def flatMap[B](f: (A1, A2) => CC[B]): CC[B] = {
      val elems1 = tuple._1.iterator
      val elems2 = tuple._2.iterator
      val b = (tuple._1: IterableTemplate[CC, A1]).newBuilder[B]
      while (elems1.hasNext && elems2.hasNext)
        b ++= f(elems1.next, elems2.next)
      b.result
    }
    def foreach[U](f: (A1, A2) => U) {
      val elems1 = tuple._1.iterator
      val elems2 = tuple._2.iterator
      while (elems1.hasNext && elems2.hasNext)
        f(elems1.next, elems2.next)
    }
    def forall(p: (A1, A2) => Boolean): Boolean = {
      val elems1 = tuple._1.iterator
      val elems2 = tuple._2.iterator
      while (elems1.hasNext && elems2.hasNext)
        if (!p(elems1.next, elems2.next)) return false
      true
    }
    def exists(p: (A1, A2) => Boolean): Boolean = {
      val elems1 = tuple._1.iterator
      val elems2 = tuple._2.iterator
      while (elems1.hasNext && elems2.hasNext)
        if (p(elems1.next, elems2.next)) return true
      false
    }
  }    
  implicit def tupleOfIterableWrapper[CC[+B] <: Iterable[B] with IterableTemplate[CC, B], A1, A2](tuple: (CC[A1], Iterable[A2])) =
    new IterableOps[CC, A1, A2](tuple)


/* A more general version which will probably not work.
  implicit def tupleOfIterableWrapper[CC[+B] <: Iterable[B] with IterableTemplate[CC, B], A1, A2, B1 <: CC[A1]](tuple: B1, Iterable[A2]) =
    new IterableOps[CC, A1, A2](tuple)
*/

  // Adriaan: If you drop the type parameters it will infer the wrong types.
  tupleOfIterableWrapper[collection.immutable.List, Int, Int]((collection.immutable.Nil, collection.immutable.Nil)) forall (_ + _ < 10)
*/
}


/** Tuple2 is the canonical representation of a @see Product2 
 *  
 */
case class Tuple2[+T1, +T2](_1:T1, _2:T2) extends Product2[T1, T2]  {
  override def toString() = {
     val sb = new StringBuilder
     sb.append('(').append(_1).append(',').append(_2).append(')')
     sb.toString
   }
  
  /** Swaps the elements of this `Tuple`.
   * @return a new Tuple where the first element is the second element of this Tuple and the
   * second element is the first element of this Tuple.
   */
  def swap: Tuple2[T2,T1] = Tuple2(_2, _1)
  
// must use <:< instead of =>, otherwise bogus any2stringadd conversion is also eligible (in case of type errors)

  type Traverserable[CC[X] <: Traversable[X], X] = TraversableClass[X, CC] with Iterable[X]

  def zip[CC[X] <: Traverserable[CC, X], A1, A2](implicit fst: T1 <:< CC[A1], snd: T2 <:< Traverserable[Iterable, A2]/*CC[A2] does not work*/): CC[(A1, A2)] = {
    map2(Tuple2((_: A1), (_: A2))) // TODO:  fix crash: 
    
  /*Exception in thread "main" scala.tools.nsc.symtab.Types$NoCommonType: lub/glb of incompatible types: [X]Tuple2.this.Traverserable[?CC,X] and CC[(A1, A2)]
     at scala.tools.nsc.symtab.Types$$anonfun$matchingBounds$1.apply(Types.scala:4729)
     ...
     at scala.tools.nsc.symtab.Types$class.solve(Types.scala:4292)
     
     should have been computing glb of:
       Tuple2.this.Traverserable[CC,(A1, A2)] and CC[(A1, A2)], with result CC[(A1, A2)]
    */
    
  }
  
  def map2[CC[X] <: Traverserable[CC, X], A1, A2, B](f: (A1, A2) => B)(implicit fst: T1 <:< CC[A1], snd: T2 <:< Traverserable[Iterable, A2]/*CC[A2] does not work*/): CC[B] = {
    val b = _1.genericBuilder[B]
    val it1 = _1.iterator
    val it2 = _2.iterator
    while (it1.hasNext && it2.hasNext)
      b += f(it1.next, it2.next)
    b.result
  }
}
