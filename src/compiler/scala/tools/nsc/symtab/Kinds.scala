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
 

trait Kinds {  // would've liked to use NominalBinding, but that would require rewriting Types (currently uses Symbol instead of Name)
  self: SymbolTable =>
  import definitions._
  
  abstract class Kind {
    def isSameKind(other: Kind): Boolean = this eq other
    def isSubKind(other: Kind): Boolean 
    def substSym(a: Symbol, b: Symbol): Kind
  }
  
  object IntervalKind {
    def apply: IntervalKind = this(mkTypeBounds(NothingClass.tpe, AnyClass.tpe))
    def apply(bounds: TypeBounds): IntervalKind = this(bounds)
    def unapply(ik: IntervalKind): Some[(TypeBounds)] = Some((ik.bounds))
  }
  
  class IntervalKind(val bounds: TypeBounds) extends Kind {
    def lowerBound=bounds.lo
    def upperBound=bounds.hi

    def substSym(a: Symbol, b: Symbol): Kind = IntervalKind(bounds.substSym(List(a),List(b)).asInstanceOf[TypeBounds]) // TODO hack
        
    override def isSameKind(other: Kind): Boolean = super.isSameKind(other) || (other match {
      case IntervalKind(otherBounds) => bounds =:=  otherBounds
      case _ => false
    } )
    
    def isSubKind(other: Kind): Boolean = isSameKind(other) || (other match {
      case IntervalKind(otherBounds) => bounds <:< otherBounds
      case _ => false
    })    
    
    override def toString 
      = if((lowerBound, upperBound)==(NothingClass.tpe, AnyClass.tpe)) "*"
        else if(lowerBound == NothingClass.tpe) "*"+ (upperBound)
        else "*"+ (lowerBound, upperBound)
  }
  
  object FunctionKind {
    object Variance {
      def apply(sym: Symbol) = 
        if (sym.isCovariant) Covariance
        else if (sym.isContravariant) Contravariance
        else Invariance      
    }
    sealed trait Variance {
      def conforms(other: Variance) = this eq other 
    }
    object Covariance extends Variance {
      override def toString = "(+)"
    }
    object Contravariance extends Variance {
      override def toString = "(-)"
    }
    object Invariance extends Variance    {
      override def conforms(other: Variance) = true
      override def toString = ""
    }
    
    case class Argument(variance: Variance, kind: Kind)(val sym: Symbol) {
      def substSym(a: Symbol, b: Symbol): Argument = Argument(variance, kind substSym(a, b))(if(sym eq a) b else sym)
      
      def isSameArg(other: Argument): Boolean 
        = (variance eq other.variance) && 
          (other.kind.substSym(other.sym, sym) isSameKind kind)

      // variance == other.variance || variance == Invariance
      // other.kind.substSym(other.sym, sym) isSubKind kind
      def conforms(other: Argument): Boolean 
        = (variance conforms other.variance) && 
          (other.kind.substSym(other.sym, sym) isSubKind kind)
    }
  }
  
  import FunctionKind._
  
  /** FunctionKind classifies a type constructor: it has N arguments. An argument consists of a binder TODO
   *  
   */
  case class FunctionKind(arg: Argument, res: Kind) extends Kind {
    def substSym(a: Symbol, b: Symbol): Kind = FunctionKind(arg substSym(a,b), res substSym(a,b))

    override def isSameKind(other: Kind): Boolean = super.isSameKind(other) || (other match {
      case FunctionKind(otherArg, otherRes) => 
          (arg isSameArg otherArg) && 
          (res isSameKind (otherRes substSym(otherArg.sym, arg.sym)))
      case _ => false
    })
        
        
    def isSubKind(other: Kind): Boolean = isSameKind(other) || (other match {
      case FunctionKind(otherArg, otherRes) => 
          (arg conforms otherArg) && 
          (res isSubKind (otherRes substSym(otherArg.sym, arg.sym)))
      case _ => false
    })    
    
    
    override def toString = "("+ arg.sym.nameString +": "+ arg.kind +"-"+ arg.variance +"->"+ res +")"
  }
  
  /**
   * Starting from a Symbol (sym) or a Type (tpe), infer the kind that classifies it (sym.tpeHK/tpe)
   * 
   * For a fully applied type (a proper type), the result will be an IntervalKind.
   *   - if the type was abstract, its bounds are used for the interval
   *   - a concrete type T gives rise to a singleton interval (lower=upper=T)
   *
   * For a higher-kinded type T with type params AT_0,...,AT_n  (recursively, AT_i has shape <v_i>p_i[AT_i_0, ..., AT_i_{n_i}], with v_i its variance)
   *  FunctionKind(arg(p_0), ... FunctionKind(arg(p_i), FunctionKind(arg(p_n), inferKind(T[p_0,...,p_n])))...)
   *  where arg(p_i) is defined as Argument(v_i, inferKind(p_i))
   */
  object inferKind {
      def apply(sym: Symbol)(pre: Type): Kind = this(sym.tpeHK)(pre, sym.owner)
      def apply(tpe: Type)(pre: Type, rootOwner: Symbol): Kind = {
        def xformAtRoot(tp: Type): Type = xform(tp, rootOwner)
        def xform(tp: Type, owner: Symbol): Type = tp.asSeenFrom(pre, owner) 
        
        if(!tpe.isHigherKinded) IntervalKind(TypeBounds(xformAtRoot(tpe.bounds.lo), xformAtRoot(tpe.bounds.hi))) // asSeenFrom's type does not specify a TypeBounds is turned into a TypeBounds, thus expand it
        else {
          val params = tpe.typeParams
          val args = params map { p => Argument(Variance(p), this(p)(pre))(p) }
          val resKind = this(appliedType(tpe, params map (_.tpeHK)))(pre, rootOwner)
          
          args.foldRight(resKind)((new FunctionKind(_, _))) 
        }
      }
    
  }
}