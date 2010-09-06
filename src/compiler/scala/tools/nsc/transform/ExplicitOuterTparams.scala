/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Adriaan Mooors
 */

package scala.tools.nsc
package transform

import symtab._
import scala.collection.mutable.ListBuffer

/*

example:

class C[TC] {
  class D[TD] {
    def m[TM] {
      def m': (TC, TD, TM)
      class E { ... TM ... }
    }
  }
}

after this phase + flatten (if classes are not flattened, e.g., on .NET, only methods need to be rewritten),
this becomes:

class C[TC]
class D[TD, TC] {
  def m[TM]
  def m'[TM]: (TC, TD, TM)
}
class E[TM, TD, TC] { ... TM ... }
*/
abstract class ExplicitOuterTparams extends InfoTransform
      with TypingTransformers
      with ast.TreeDSL {
  import global._
  import definitions._

  /** the name of the phase: */
  val phaseName: String = "explicitoutertparams"

  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new Phase(prev)
  class Phase(prev: scala.tools.nsc.Phase) extends super.Phase(prev) {
    override def substInClassInfoTypes = true      
  }

  /** This class does not change linearization */
  override def changesBaseClasses = false

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new ExplicitOuterTparamsTransformer(unit)

  def isNestedInMethod(sym: Symbol) = sym.owner.isMethod || (owner != NoSymbol && !owner.isClass && isNestedInMethod(sym.owner)) // do we need the second half?

  // concat of tparams in sym's ownerChain, once removed (excluding sym's owner's type params so we don't have to subst those)
  // for methods, we stop at the first enclosing method
  def outerTparamRefs(sym: Symbol): List[Symbol] =
    (if(isNestedInMethod(sym)) sym.owner.ownerChain.takeWhile(!_.isClass) // methods will not be lifted out of their enclosing class, but will leave their outer methods
     else sym.owner.ownerChain) flatMap (_.typeParams)

//  def isPotentiallyPolymorphicClass(sym: Symbol): Boolean = sym.isClass && !(sym.hasFlag(MODULE) || sym.hasFlag(PACKAGE))
  def transformInfo(sym: Symbol, tp: Type): Type = if(sym.owner != NoSymbol) { // no need to add outers if we're toplevel
    val outerRefs = outerTparamRefs(sym)
    if(outerRefs nonEmpty) addOuterTParams(sym, outerRefs)(tp) // no need to add outer tparams if no outer is polymorphic
    else tp
  } else tp

  def addOuterTParams(sym: Symbol, outerRefs: List[Symbol])(tp: Type) = {
    lazy val outers = outerRefs map (_.newOuterTparam(sym))

    tp match {
      case PolyType(tps, res) =>
        PolyType(tps ++ outers, res.subst(outerRefs, outers))
      case ClassInfoType(parents, decls, clazz) => // monomorphic class becomes polymorphic if some of its outers are
        PolyType(outers, tp.subst(outerRefs, outers))
      case MethodType(_, _) =>
        PolyType(outers, tp.subst(outerRefs, outers))
      case _ =>
        tp.subst(outerRefs, outers)
    }
  }

  class ExplicitOuterTparamsTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    /** The main transformation method */
    override def transform(tree: Tree): Tree = tree match {
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) if tree.symbol.typeParams.length != tparams.length => // transformInfo added type params
        val newParams = tree.symbol.typeParams.drop(tparams.length) // new ones are tacked on at the end
        val newParamTrees: List[Tree] = List()
        super.transform(treeCopy.DefDef(tree, mods, name, tparams ++ newParamTrees, vparamss, tpt, rhs))
      // case classdef
      // case select --> add type param refs when mono became poly
      // case typeapply --> add further type param refs
      case _ =>
        val res = super.transform(tree)
        if (res.tpe ne null) res setType transformInfo(currentOwner, res.tpe)
        res
    }

    /** The transformation method for whole compilation units */
    // override def transformUnit(unit: CompilationUnit) {
    //   cunit = unit
    //   atPhase(phase.next) { super.transformUnit(unit) }
    //   cunit = null
    // }
  }

}
