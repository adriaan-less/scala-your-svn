/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// $Id: Traversable.scala 15188 2008-05-24 15:01:02Z stepancheg $
package scala.collection.generic

trait TraversableClass[+A, +CC[X] <: Traversable[X]] {

  /** The factory companion object that builds instances of class CC */

  def companion: Companion[CC]
  /** The builder that builds instances of CC[A] */

  protected[this] def newBuilder: Builder[A, CC[A]] = companion.newBuilder[A]

  /** The generic builder that builds instances of CC at arbitrary element types. */
  def genericBuilder[B]: Builder[B, CC[B]] = companion.newBuilder[B]
  
  // @M TODO: redundant... (but a self type like CC[A] does not type check)
  def foreach[U](f: A => U): Unit 
    
  implicit def subtype_witness[T <: U, U](x: T): U = x  // @M TODO: move to Predef? (+ optimiser should eliminate it)
  def unzip[X, Y](implicit c: A => (X, Y)): (CC[X], CC[Y]) = {
    val b1 = genericBuilder[X]
    val b2 = genericBuilder[Y]
    foreach { case (x1: X, x2: Y) =>
      b1 += x1
      b2 += x2
    }
    (b1.result, b2.result)    
  }
}

