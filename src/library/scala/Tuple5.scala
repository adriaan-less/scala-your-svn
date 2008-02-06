
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

// generated by genprod on Tue Feb 05 21:30:06 CET 2008

package scala

/** Tuple5 is the canonical representation of a @see Product5 */
case class Tuple5[+T1, +T2, +T3, +T4, +T5](_1:T1, _2:T2, _3:T3, _4:T4, _5:T5) 
  extends Product5[T1, T2, T3, T4, T5]  {

   override def toString() = {
     val sb = new StringBuilder
     sb.append('(').append(_1).append(',').append(_2).append(',').append(_3).append(',').append(_4).append(',').append(_5).append(')')
     sb.toString
   }
}
