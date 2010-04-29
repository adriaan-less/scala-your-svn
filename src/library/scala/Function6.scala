/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

// generated by genprod on Thu Apr 29 17:52:16 CEST 2010  (with extra methods)

package scala




/** <p>
 *    Function with 6 parameters.
 *  </p>
 *  
 */
trait Function6[-T1, -T2, -T3, -T4, -T5, -T6, +R] extends AnyRef { self =>
  def apply(v1:T1,v2:T2,v3:T3,v4:T4,v5:T5,v6:T6): R
  override def toString() = "<function6>"
  
  /** f(x1, x2, x3, x4, x5, x6)  == (f.curried)(x1)(x2)(x3)(x4)(x5)(x6)
   */
  def curried: T1 => T2 => T3 => T4 => T5 => T6 => R = {
    (x1: T1) => ((x2: T2, x3: T3, x4: T4, x5: T5, x6: T6) => self.apply(x1, x2, x3, x4, x5, x6)).curried
  }
  @deprecated("Use 'curried' instead")
  def curry = curried

  /* f(x1, x2, x3, x4, x5, x6) == (f.tupled)(Tuple6(x1, x2, x3, x4, x5, x6))
   */
  def tupled: Tuple6[T1, T2, T3, T4, T5, T6] => R = {
    case Tuple6(x1, x2, x3, x4, x5, x6) => apply(x1, x2, x3, x4, x5, x6)
  }

}
