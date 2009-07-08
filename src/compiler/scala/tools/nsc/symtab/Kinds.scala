/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Adriaan Moors
 */
// 

package scala.tools.nsc.symtab

import scala.collection.immutable
import scala.tools.nsc.util.Position

trait Kinds {  // would've liked to use NominalBinding, but that would require rewriting Types (currently uses Symbol instead of Name)
  self: SymbolTable =>
  import definitions._
  
  abstract class Kind {
    def map(f: Type => Type): Kind
    def isSubKind(other: Kind): Boolean 
    def substSym(a: List[Symbol], b: List[Symbol]): Kind
  }
  
  object IntervalKind {
    def apply: IntervalKind = this(mkTypeBounds(NothingClass.tpe, AnyClass.tpe))
    def apply(bounds: TypeBounds): IntervalKind = new IntervalKind(bounds)
    def unapply(ik: IntervalKind): Some[(TypeBounds)] = Some((ik.bounds))
  }
  
  class IntervalKind(val bounds: TypeBounds) extends Kind {
    def lowerBound=bounds.lo
    def upperBound=bounds.hi

    def map(f: Type => Type): IntervalKind = IntervalKind(mkTypeBounds(f(lowerBound), f(upperBound)))
    def substSym(a: List[Symbol], b: List[Symbol]): Kind = IntervalKind(bounds.substSym(a, b).asInstanceOf[TypeBounds]) // TODO hack
        
    def isSubKind(other: Kind): Boolean = other match {
      case IntervalKind(otherBounds) => bounds <:< otherBounds //  println("ISK_IK"+(bounds, otherBounds)); //@MDEBUG
      case _ => false
    }
    
    override def toString 
      = if((lowerBound, upperBound)==(NothingClass.tpe, AnyClass.tpe)) "*"
        else if(lowerBound == NothingClass.tpe) "*("+ upperBound +")"
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
      override def conforms(other: Variance) = true // an invariance annotation may safely be upcast to co/contra-variance
      override def toString = ""
    }
    
    case class Argument(variance: Variance, kind: Kind)(val sym: Symbol) {
      private def substMe(from: List[Symbol], to: List[Symbol]): Symbol =
        (from, to) match { case (f :: frest, t :: trest)  => if (f eq sym) t else substMe(frest, trest) case _ => sym}
      
      def map(f: Type => Type): Argument = Argument(variance, kind map(f))(sym)
      def substSym(a: List[Symbol], b: List[Symbol]): Argument = Argument(variance, kind substSym(a, b))(substMe(a, b))
      
      // variance == other.variance || variance == Invariance
      // other.kind.substSym(other.sym, sym) isSubKind kind
      def conforms(other: Argument): Boolean 
        = (variance conforms other.variance) && 
          (other.kind.substSym(List(other.sym), List(sym)) isSubKind kind) // contravariant 
           // TODO check that we need not generate fresh sym and substitute other.sym and sym to the fresh one
           //      see ticket 2101 for analogous bug 
           // if this situation can arise (note occurrence of X in kinds)
           //  X : *(Nothing, Ordered[X]) -(.)->  ...   <:   Y : *(Nothing, Ordered[X]) -(.)-> ...
           // we would get a false positive 
           // it seems this cannot happen for now
    }
  }
  
  import FunctionKind._
  
  /** FunctionKind classifies a type constructor: it has N arguments. An argument consists of a binder TODO
   *  
   */
  case class FunctionKind(arg: Argument, res: Kind) extends Kind {
    def map(f: Type => Type): FunctionKind = FunctionKind(arg map(f), res map(f))
    def substSym(a: List[Symbol], b: List[Symbol]): Kind = FunctionKind(arg substSym(a,b), res substSym(a,b))
    
    /**
     * this             isSubKind    other           (for this and other FunctionKind's) if:
     * (X1: ka1) -(v1)-> kr1     <:        (X2: ka2) -(v2)-> kr2   if:
     *   ([X2 |-> X1] ka2) <: ka1
     *   v1 <: v2  (where <: on variance is the smallest reflexive and transitive relation that includes 0 <: + and 0 <: -)
     *   kr1 <: ([X2 |-> X1] kr2)
     */    
    def isSubKind(other: Kind): Boolean = other match {
      case FunctionKind(otherArg, otherRes) =>  //println("ISK: "+(arg, otherArg, res, otherRes substSym(List(otherArg.sym), List(arg.sym)))) //@MDEBUG
          (arg conforms otherArg) && 
          (res isSubKind (otherRes substSym(List(otherArg.sym), List(arg.sym)))) // covariant
      case _ => false
    }
    
    
    override def toString = "("+ arg.sym.nameString +": "+ arg.kind +" -"+ arg.variance +"-> "+ res +")"
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
      def apply(pre: Type): InferKind = new InferKind {
        def apply(tpe: Type, owner: Symbol): Kind = {
          if(!tpe.isHigherKinded) 
            IntervalKind(tpe.asSeenFrom(pre, owner).bounds)  
            // no need to substitute params to args here (which is necessary in isWithinBounds)
            // for kinds, this substitution is performed in isSubKind
          else {
            val params = tpe.typeParams
            val args = params map { p => Argument(Variance(p), apply(p))(p) }
            val resKind = apply(appliedType(tpe, params map (_.tpeHK)), owner)
          
            args.foldRight(resKind)((new FunctionKind(_, _))) 
          }
        }
      }
      
      abstract class InferKind {
        def apply(tpe: Type, owner: Symbol): Kind
        def apply(sym: Symbol): Kind = apply(sym.tpeHK, sym.owner)
      }
  }

  // check whether type application is well-kinded
  def checkKinds(pos: Position, pre: Type, owner: Symbol, 
                  tparams: List[Symbol], targs: List[Type], prefix: String, error: (Position, String)=>Unit) = {
                    
    val kindErrors = (tparams zip targs) flatMap {case (param, arg) => 
       val argK = inferKind(pre)(arg, owner) 
       val parK = inferKind(pre)(param) map (_.instantiateTypeParams(tparams, targs))
       if(argK isSubKind parK) List()
       else List(arg +" has kind "+ argK +", but expected kind "+ parK +" for "+ param)}
  
    if(!kindErrors.isEmpty)
      error(pos, 
           prefix +"incompatible kinds in arguments supplied to "+ tparams.head.owner +": "+ 
           kindErrors.mkString("", ";\n", "."))
  }

    //     //println("bounds = "+bounds+", targs = "+targs+", targclasses = "+(targs map (_.getClass))+", parents = "+(targs map (_.parents)))
    //     //println(List.map2(bounds, targs)((bound, targ) => bound containsType targ))
    //     if (settings.explaintypes.value) {
    //       val bounds = tparams map (tp => tp.info.instantiateTypeParams(tparams, targs).bounds)
    //       List.map2(targs, bounds)((targ, bound) => explainTypes(bound.lo, targ))
    //       List.map2(targs, bounds)((targ, bound) => explainTypes(targ, bound.hi))
    //       ()
    //     }
    //   }
    // }
   
}