/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: AllRef$.java 5880 2006-03-02 23:05:17Z mihaylov $


package scala


/**
 * @deprecated <i>To be removed at some time in the future. Kept only for
 * backward compatibility. Newly compiled code will refer to
 * <code>scala.runtime.Null$</code></i>
 *
 * Dummy class which exist only to satisfy the JVM. It corresponds
 * to <code>scala.AllRef</code>. If such type appears in method
 * signatures, it is erased to this one.
 */

sealed abstract class AllRef$
