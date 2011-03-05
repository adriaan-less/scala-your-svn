/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection

import generic._
import mutable.{ Builder, SetBuilder }
import scala.annotation.migration

/** A template trait for sets.
 *
 *  $setNote
 *  $setTags
 *  @since 2.8
 *
 *  @define setNote
 *
 *  A set is a collection that contains no duplicate elements.
 *  
 *    '''Implementation note:'''
 *    This trait provides most of the operations of a `Set` independently of its representation.
 *    It is typically inherited by concrete implementations of sets.
 *
 *    To implement a concrete set, you need to provide implementations of the
 *    following methods:
 *    {{{
 *       def contains(key: A): Boolean
 *       def iterator: Iterator[A]
 *       def +(elem: A): This
 *       def -(elem: A): This
 *    }}}
 *    If you wish that methods like `take`, `drop`,
 *    `filter` return the same kind of set, you should also override:
 *    {{{
 *       def empty: This
 *    }}}
 *    It is also good idea to override methods `foreach` and
 *    `size` for efficiency.
 * 
 * @define setTags
 *  @tparam A    the type of the elements of the set
 *  @tparam This the type of the set itself.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 * 
 *  @define coll set
 *  @define Coll Set
 *  @define willNotTerminateInf
 *  @define mayNotTerminateInf
 */
trait SetLike[A, +This <: SetLike[A, This] with Set[A]] 
extends IterableLike[A, This] 
   with Subtractable[A, This] { 
self =>

  /** The empty set of the same type as this set
   * @return  an empty set of type `This`.
   */
  def empty: This

  /** A common implementation of `newBuilder` for all sets in terms
   *  of `empty`. Overridden for mutable sets in
   *  <a href="mutable/SetLike.html" target="ContentFrame">
   *  `mutable.SetLike`</a>.
   */
  override protected[this] def newBuilder: Builder[A, This] = new SetBuilder[A, This](empty)
  
  /** Overridden for efficiency. */
  override def toSeq: Seq[A] = toBuffer[A]
  override def toBuffer[A1 >: A]: mutable.Buffer[A1] = {
    val result = new mutable.ArrayBuffer[A1](size)
    copyToBuffer(result)
    result
  }

  // note: this is only overridden here to add the migration annotation,
  // which I hope to turn into an Xlint style warning as the migration aspect
  // is not central to its importance.
  @migration(2, 8, "Set.map now returns a Set, so it will discard duplicate values.")
  override def map[B, That](f: A => B)(implicit bf: CanBuildFrom[This, B, That]): That = super.map(f)(bf)

  /** Tests if some element is contained in this set.
   *
   *  @param elem the element to test for membership.
   *  @return     `true` if `elem` is contained in this set, `false` otherwise.
   */
  def contains(elem: A): Boolean

  /** Creates a new set with an additional element, unless the element is
   *  already present.
   *
   *  @param elem the element to be added
   *  @return a new set that contains all elements of this set and that also
   *          contains `elem`.
   */
  def + (elem: A): This
  
  /** Creates a new $coll with additional elements. 
   *
   *  This method takes two or more elements to be added. Another overloaded
   *  variant of this method handles the case where a single element is added.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   *  @return   a new $coll with the given elements added.
   */
  def + (elem1: A, elem2: A, elems: A*): This = this + elem1 + elem2 ++ elems
  
  /** Creates a new $coll by adding all elements contained in another collection to this $coll.
   *
   *  @param elems     the collection containing the added elements.
   *  @return a new $coll with the given elements added.
   */
  def ++ (elems: TraversableOnce[A]): This = newBuilder ++= this ++= elems result

  /** Creates a new set with a given element removed from this set.
   *
   *  @param elem the element to be removed
   *  @return a new set that contains all elements of this set but that does not
   *          contain `elem`.
   */
  def - (elem: A): This

  /** Tests if this set is empty.
   *
   *  @return `true` if there is no element in the set, `false` otherwise.
   */
  override def isEmpty: Boolean = size == 0

  /** Tests if some element is contained in this set.
   *
   *  This method is equivalent to `contains`. It allows sets to be interpreted as predicates.
   *  @param elem the element to test for membership.
   *  @return  `true` if `elem` is contained in this set, `false` otherwise.
   */
  def apply(elem: A): Boolean = this contains elem

  /** Computes the intersection between this set and another set.
   *
   *  @param   that  the set to intersect with.
   *  @return  a new set consisting of all elements that are both in this
   *  set and in the given set `that`. 
   */
  def intersect(that: Set[A]): This = this filter that

  /** Computes the intersection between this set and another set.
   *
   *  '''Note:'''  Same as `intersect`.
   *  @param   that  the set to intersect with.
   *  @return  a new set consisting of all elements that are both in this
   *  set and in the given set `that`. 
   */
  def &(that: Set[A]): This = this intersect that

 /**  This method is an alias for `intersect`. 
   *  It computes an intersection with set `that`.
   *  It removes all the elements that are not present in `that`.
   *
   *  @param that the set to intersect with
   */
  @deprecated("use & instead")
  def ** (that: Set[A]): This = &(that)
  
  /** Computes the union between of set and another set.
   *
   *  @param   that  the set to form the union with.
   *  @return  a new set consisting of all elements that are in this
   *  set or in the given set `that`. 
   */
  def union(that: Set[A]): This = this ++ that

  /** Computes the union between this set and another set.
   *
   *  '''Note:'''  Same as `union`.
   *  @param   that  the set to form the union with.
   *  @return  a new set consisting of all elements that are in this
   *  set or in the given set `that`. 
   */
  def | (that: Set[A]): This = this union that

  /** Computes the difference of this set and another set.
   *
   *  @param that the set of elements to exclude.
   *  @return     a set containing those elements of this
   *              set that are not also contained in the given set `that`.
   */
  def diff(that: Set[A]): This = this -- that

  /** The difference of this set and another set.
   *
   *  '''Note:'''  Same as `diff`.
   *  @param that the set of elements to exclude.
   *  @return     a set containing those elements of this
   *              set that are not also contained in the given set `that`.
   */
  def &~(that: Set[A]): This = this diff that

  /** Tests whether this set is a subset of another set.
   *
   *  @param that  the set to test.
   *  @return     `true` if this set is a subset of `that`, i.e. if
   *              every element of this set is also an element of `that`.
   */
  def subsetOf(that: Set[A]) = this forall that
  
  /** An iterator over all subsets of this set of the given size.
   *  If the requested size is impossible, an empty iterator is returned.
   *
   *  @param len  the size of the subsets.
   *  @return     the iterator.
   */
  def subsets(len: Int): Iterator[This] = {
    if (len < 0 || len > size) Iterator.empty
    else new SubsetsItr(self.toIndexedSeq, len)
  }

  /** An iterator over all subsets of this set.
   *
   *  @return     the iterator.
   */
  def subsets: Iterator[This] = new Iterator[This] {
    private val elms = self.toIndexedSeq
    private var len = 0
    private var itr: Iterator[This] = Iterator.empty

    def hasNext = len <= elms.size || itr.hasNext
    def next = {
      if (!itr.hasNext) {
        if (len > elms.size) Iterator.empty.next
        else {
          itr = new SubsetsItr(elms, len)
          len += 1
        }
      }
      
      itr.next
    }
  }
    
  /** An Iterator include all subsets containing exactly len elements.
   *  If the elements in 'This' type is ordered, then the subsets will also be in the same order.
   *  ListSet(1,2,3).subsets => {1},{2},{3},{1,2},{1,3},{2,3},{1,2,3}}
   *
   *  @author Eastsun
   *  @date 2010.12.6
   */
  private class SubsetsItr(elms: IndexedSeq[A], len: Int) extends Iterator[This] {
    private val idxs = Array.range(0, len+1)
    private var _hasNext = true
    idxs(len) = elms.size

    def hasNext = _hasNext
    def next: This = {
      if (!hasNext) Iterator.empty.next

      val buf = self.newBuilder
      idxs.slice(0, len) foreach (idx => buf += elms(idx))
      val result = buf.result

      var i = len - 1
      while (i >= 0 && idxs(i) == idxs(i+1)-1) i -= 1

      if (i < 0) _hasNext = false
      else {
        idxs(i) += 1
        for (j <- (i+1) until len)
          idxs(j) = idxs(j-1) + 1
      }
      
      result
    }
  }

  /** Defines the prefix of this object's `toString` representation.
   *  @return  a string representation which starts the result of `toString` applied to this set.
   *           Unless overridden this is simply `"Set"`.
   */
  override def stringPrefix: String = "Set"
  override def toString = super[IterableLike].toString

  // Careful! Don't write a Set's hashCode like:
  //    override def hashCode() = this map (_.hashCode) sum
  // Calling map on a set drops duplicates: any hashcode collisions would
  // then be dropped before they can be added.
  // Hash should be symmetric in set entries, but without trivial collisions.
  override def hashCode() = util.MurmurHash.symmetricHash(this,Set.hashSeed)
  
  /** Compares this set with another object for equality.
   *
   *  '''Note:''' This operation contains an unchecked cast: if `that`
   *        is a set, it will assume with an unchecked cast
   *        that it has the same element type as this set.
   *        Any subsequent ClassCastException is treated as a `false` result.
   *  @param that the other object
   *  @return     `true` if `that` is a set which contains the same elements
   *              as this set.
   */
  override def equals(that: Any): Boolean = that match {
    case that: Set[_] =>
      (this eq that) ||
      (that canEqual this) &&
      (this.size == that.size) &&
      (try this subsetOf that.asInstanceOf[Set[A]]
       catch { case ex: ClassCastException => false })
    case _ =>
      false
  }
}
