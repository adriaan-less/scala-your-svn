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
import scala.collection.generic.GenericTraversableTemplate
import scala.collection.mutable.Builder


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
  
  type Traverserable[CC[X] <: Traversable[X], X] = GenericTraversableTemplate[X, CC] with Iterable[X]

  // TODO: benchmark factored version vs inlining forall2 everywhere (specialisation?)
  // factor further? (use fold2)
  // must use <:< instead of =>, otherwise bogus any2stringadd conversion is also eligible (in case of type errors)
  
  
  def forall2[CC[X] <: Traverserable[CC, X], A1, A2](f: (A1, A2) => Boolean)(implicit fst: T1 <:< CC[A1], snd: T2 <:< Traverserable[Iterable, A2]/*CC[A2] does not work*/): Boolean = {
    val it1 = _1.iterator
    val it2 = _2.iterator
    var res = true
    while (res && it1.hasNext && it2.hasNext)
      res = f(it1.next, it2.next)
    res
  }
  
  def exists2[CC[X] <: Traverserable[CC, X], A1, A2](f: (A1, A2) => Boolean)(implicit fst: T1 <:< CC[A1], snd: T2 <:< Traverserable[Iterable, A2]/*CC[A2] does not work*/): Boolean = {
    val it1 = _1.iterator
    val it2 = _2.iterator
    var res = false
    while (!res && it1.hasNext && it2.hasNext)
      res = f(it1.next, it2.next)
    res
  }
  
  def foreach2[CC[X] <: Traverserable[CC, X], A1, A2, U](f: (A1, A2) => U)(implicit fst: T1 <:< CC[A1], snd: T2 <:< Traverserable[Iterable, A2]/*CC[A2] does not work*/): Unit 
    = forall2[CC, A1, A2]{(x, y) => f(x, y); true} // XXX: remove type args and fix crash in type infer

  def build2[CC[X] <: Traverserable[CC, X], A1, A2, B](f: Builder[B, CC[B]] => (A1, A2) => Unit)(implicit fst: T1 <:< CC[A1], snd: T2 <:< Traverserable[Iterable, A2]/*CC[A2] does not work*/): CC[B] = {
    val b = _1.genericBuilder[B]
      foreach2[CC, A1, A2, Unit](f(b)) // XXX: remove type args and fix crash in type infer
    b.result
  }

  def zip2[CC[X] <: Traverserable[CC, X], A1, A2](implicit fst: T1 <:< CC[A1], snd: T2 <:< Traverserable[Iterable, A2]/*CC[A2] does not work*/): CC[(A1, A2)] 
    = build2[CC, A1, A2, (A1, A2)]{b => (x, y) =>  // XXX: remove type args and fix crash in type infer
        b += Tuple2(x, y)
      }
  
  def map2[CC[X] <: Traverserable[CC, X], A1, A2, B](f: (A1, A2) => B)(implicit fst: T1 <:< CC[A1], snd: T2 <:< Traverserable[Iterable, A2]/*CC[A2] does not work*/): CC[B] 
    = build2[CC, A1, A2, B]{b => (x, y) =>  // XXX: remove type args and fix crash in type infer
        b += f(x, y)
      }

  def flatMap2[CC[X] <: Traverserable[CC, X], A1, A2, B](f: (A1, A2) => CC[B])(implicit fst: T1 <:< CC[A1], snd: T2 <:< Traverserable[Iterable, A2]/*CC[A2] does not work*/): CC[B] 
    = build2[CC, A1, A2, B]{b => (x, y) =>  // XXX: remove type args and fix crash in type infer
        b ++= f(x, y)
      }

    
}
