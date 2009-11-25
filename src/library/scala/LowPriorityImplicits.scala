/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import collection.mutable._
import collection.immutable.WrappedString
import collection.generic.CanBuildFrom

class LowestPriorityImplicits { // should not contain other implicit values of type `? => ?`
  // used, for example, in the encoding of generalized constraints
  // we need a new type constructor `<:<` and evidence `conforms`, as 
  // reusing `Function2` and `identity` leads to ambiguities (any2stringadd is inferred)
  // to constrain any abstract type T that's in scope in a method's argument list (not just the method's own type parameters)
  // simply add an implicit argument of type `T <:< U`, where U is the required upper bound (for lower-bounds, use: `U <:< T`)
  // in part contributed by Jason Zaugg
  // has to be last resort
  sealed abstract class <:<[-From, +To] extends (From => To)
  implicit def conforms[A]: A <:< A = new (A <:< A) {def apply(x: A) = x} // not in the <:< companion object because it is also intended to subsume identity (which is no longer implicit)
  // rename to `conforms`
}
/** The `LowPriorityImplicits` class provides implicit values that
 *  are valid in all Scala compilation units without explicit qualification,
 *  but that are partially overridden by higher-priority conversions in object
 *  `Predef`.
 *
 *  @author  Martin Odersky
 *  @since 2.8
 */
class LowPriorityImplicits extends LowestPriorityImplicits {

  implicit def genericWrapArray[T](xs: Array[T]): WrappedArray[T] = 
    WrappedArray.make(xs)

  implicit def wrapRefArray[T <: AnyRef](xs: Array[T]): WrappedArray[T] = new WrappedArray.ofRef[T](xs)
  implicit def wrapIntArray(xs: Array[Int]): WrappedArray[Int] = new WrappedArray.ofInt(xs)
  implicit def wrapDoubleArray(xs: Array[Double]): WrappedArray[Double] = new WrappedArray.ofDouble(xs)
  implicit def wrapLongArray(xs: Array[Long]): WrappedArray[Long] = new WrappedArray.ofLong(xs)
  implicit def wrapFloatArray(xs: Array[Float]): WrappedArray[Float] = new WrappedArray.ofFloat(xs)
  implicit def wrapCharArray(xs: Array[Char]): WrappedArray[Char] = new WrappedArray.ofChar(xs)
  implicit def wrapByteArray(xs: Array[Byte]): WrappedArray[Byte] = new WrappedArray.ofByte(xs)
  implicit def wrapShortArray(xs: Array[Short]): WrappedArray[Short] = new WrappedArray.ofShort(xs)
  implicit def wrapBooleanArray(xs: Array[Boolean]): WrappedArray[Boolean] = new WrappedArray.ofBoolean(xs)
  implicit def wrapUnitArray(xs: Array[Unit]): WrappedArray[Unit] = new WrappedArray.ofUnit(xs)

  implicit def wrapString(s: String): WrappedString = new WrappedString(s)
  implicit def unwrapString(ws: WrappedString): String = ws.self

  implicit def fallbackStringCanBuildFrom[T]: CanBuildFrom[String, T, collection.immutable.IndexedSeq[T]] = 
    new CanBuildFrom[String, T, collection.immutable.IndexedSeq[T]] { 
      def apply(from: String) = scala.collection.immutable.IndexedSeq.newBuilder[T]
      def apply() = scala.collection.immutable.IndexedSeq.newBuilder[T]
    }

  /** Can go away after next newstarr */
  def wrapArray[T <: AnyRef](xs: Array[T]): WrappedArray[T] = new WrappedArray.ofRef[T](xs)
  def wrapArray(xs: Array[Int]): WrappedArray[Int] = new WrappedArray.ofInt(xs)
  def wrapArray(xs: Array[Double]): WrappedArray[Double] = new WrappedArray.ofDouble(xs)
  def wrapArray(xs: Array[Long]): WrappedArray[Long] = new WrappedArray.ofLong(xs)
  def wrapArray(xs: Array[Float]): WrappedArray[Float] = new WrappedArray.ofFloat(xs)
  def wrapArray(xs: Array[Char]): WrappedArray[Char] = new WrappedArray.ofChar(xs)
  def wrapArray(xs: Array[Byte]): WrappedArray[Byte] = new WrappedArray.ofByte(xs)
  def wrapArray(xs: Array[Short]): WrappedArray[Short] = new WrappedArray.ofShort(xs)
  def wrapArray(xs: Array[Boolean]): WrappedArray[Boolean] = new WrappedArray.ofBoolean(xs)
  def wrapArray(xs: Array[Unit]): WrappedArray[Unit] = new WrappedArray.ofUnit(xs)
}
