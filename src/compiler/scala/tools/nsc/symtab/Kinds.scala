/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Adriaan Moors
 */
// 

package scala.tools.nsc.symtab

import scala.collection.immutable
// import scala.collection.mutable.{ListBuffer, HashMap, WeakHashMap}
// import scala.tools.nsc.util.{HashSet, Position, NoPosition}
// import Flags._
 

trait Kinds { 
  self: SymbolTable =>
  import definitions._
  
  abstract class Kind {
    
  }
  
  object IntervalKind {
    def apply: IntervalKind = this(NothingClass.tpe, AnyClass.tpe)
    def apply(upperBound: Type): IntervalKind = this(NothingClass.tpe, upperBound)
    def apply(lowerBound: Type, upperBound: Type): IntervalKind = new IntervalKind(lowerBound, upperBound)
    def unapply(ik: IntervalKind): Some[(Type, Type)] = Some((ik.lowerBound, ik.upperBound))
  }
  
  class IntervalKind(val lowerBound: Type, val upperBound: Type) extends Kind {
    
  }
  
  abstract class FunctionKind extends Kind {
    
  }
  
}