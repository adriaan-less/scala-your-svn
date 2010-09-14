/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Adriaan Mooors
 */

package scala.tools.nsc
package transform

import symtab._
import scala.collection.mutable.ListBuffer

/** This phase prepares for flattening after erasure, but before type parameters are removed (which erasure doesn't do anymore).

The transformation anticipates:
 - classes being lifted out of their outer class/method, to the top,
 - methods being lifted out of their outers up to the next enclosing class (thus, only methods nested in methods are affected)

After this lifting, type parameters of the outer classes are no longer available, so we need to add them to all
classes and methods that will be lifted. To enable modular compilation, we simply add all type parameters of all outers
that will no longer be visible after lifting (hence, we follow the owner chain to the root for classes,
but stop at the immediately enclosing class for methods)

Example:

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

class C[TC] // here, references to D[TD] get replaced by D[TD, TC], where TC refers to the original symbol
class D[TD, TC] { // in this scope, the old TC symbol is replaced by the cloned TC symbol, similarly for the other cloned outers
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
//    override val keepsTypeParams = false // leads to cyclic errors because this means we look at a class's type params while transforming its info
    // keepTypeParams == false means unsafeTypeParams does not cross this type history entry when retrieving a symbol's info
    override def substInClassInfoTypes = true
  }

  /** This class does not change linearization */
  override def changesBaseClasses = false

  protected def newTransformer(unit: CompilationUnit): Transformer = new ExplicitOuterTparamsTransformer(unit)

  def isNestedInMethod(sym: Symbol): Boolean = sym.owner.isMethod || (sym.owner != NoSymbol && !sym.owner.isClass && isNestedInMethod(sym.owner)) // do we need the second half?

  // concat of tparams in sym's ownerChain, once removed (excluding sym's owner's type params so we don't have to subst those)
  // for methods, we stop at the first enclosing method
  // @requires affectedByFlatten(sym)
  def outerTparamRefs(sym: Symbol): List[Symbol] =
    (if(sym.isMethod) sym.owner.ownerChain.takeWhile(!_.isClass) // methods will not be lifted out of their enclosing class, but will leave their outer methods
     else sym.owner.ownerChain) flatMap (_.typeParams)

  def affectedByFlatten(sym: Symbol): Boolean = {
    sym.owner != NoSymbol &&
      (  sym.isNestedClass
      || (sym.isMethod && isNestedInMethod(sym))) // method nested in method -- if directly defined in class, it will not get lifted out
  }

  // add references to outer type params -- need to subst them to cloned ones in scope of the inner class
  //  cloning is done in addOuterTparams
  //  substing is done by the tree transformer
  object transformType extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case TypeRef(pre, sym, args) if affectedByFlatten(sym) =>
        typeRef(this(pre), sym, mapOverArgs(args, sym.typeParams) ++ outerTparamRefs(sym).map(p => pre.memberType(p))) // TODO: p.tpeHK ?
      case ThisType(sym) if affectedByFlatten(sym) => // must expand so we get a chance to add type arguments, erasure would do the expansion in any case 
        this(tp.underlying)
      case _ => mapOver(tp)
    }
  }

  //  def isPotentiallyPolymorphicClass(sym: Symbol): Boolean = sym.isClass && !(sym.hasFlag(MODULE) || sym.hasFlag(PACKAGE))
  def transformInfo(sym: Symbol, tp: Type): Type = if(affectedByFlatten(sym)) {
    val outerRefs = outerTparamRefs(sym)
    if(outerRefs nonEmpty) addOuterTParams(sym, outerRefs)(tp)
    else transformType(tp) // no need to add outer tparams if no outer is polymorphic, but vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  } else transformType(tp) // not affected by the later flatten phase, but its type may still refer to a symbol that is affected

  def addOuterTParams(sym: Symbol, outerRefs: List[Symbol])(tp: Type) = {
    val outers = outerRefs map (_.cloneSymbol(sym))
    def subst(tp: Type) = {
      // TODO if outerRefs exists (_.owner.isMethod) ==> deal with skolemization
      val res = tp.substSym(outerRefs, outers) // should go into classinfotypes
      println("adding outer params to "+ sym +" outers: "+(outerRefs, outers)+" res: "+res)
      res
    }

    tp match {
      case PolyType(tps, res) =>
        PolyType(tps ++ outers, subst(res))
      case ClassInfoType(parents, decls, clazz) => // monomorphic class becomes polymorphic if some of its outers are
        PolyType(outers, subst(tp))
      case MethodType(_, _) =>
        PolyType(outers, subst(tp))
      case _ =>
        subst(tp)
    }
  }

  class ExplicitOuterTparamsTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    // these refs point to the original type parameters, so they are only valid outside of the definition of the symbol referenced by tree
    // inside of the definition, refs to outer type params should be substituted to the new outer tparams
    // --> this happens in transformInfo, should we also do in in the tree transformer here?
    private def targTrees(tree: Tree): List[Tree] = {
      outerTparamRefs(tree.symbol).map(tparam => TypeTree(tparam.tpeHK))
    }

    private def tdefTrees(tparams: List[Symbol]): List[TypeDef] = {
      tparams.map(TypeDef(_))
    }

    /** The main transformation method */
    override def transform(tree: Tree): Tree = tree match {
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) if affectedByFlatten(tree.symbol) =>
        // TODO: (re)skolemization
        val oldParams = outerTparamRefs(tree.symbol) drop tparams.length
        val newParams = (atPhase(phase.next) { tree.symbol.info.typeParams }) drop tparams.length
        println("new tparams for "+ name +": "+ newParams)
        transformSubst(oldParams, newParams)(treeCopy.DefDef(tree, mods, name, tparams ++ tdefTrees(newParams), vparamss, tpt,rhs))
      case ClassDef(mods, name, tparams, impl) if affectedByFlatten(tree.symbol) =>
        val oldParams = outerTparamRefs(tree.symbol) drop tparams.length
        val newParams = (atPhase(phase.next) { tree.symbol.info.typeParams }) drop tparams.length
        println("new tparams for class "+ name +": "+ newParams)
        transformSubst(oldParams, newParams)(treeCopy.ClassDef(tree, mods, name, tparams ++ tdefTrees(newParams), impl))
      case ModuleDef(mods, name, impl) if affectedByFlatten(tree.symbol) =>
        val newParams = (atPhase(phase.next) { tree.symbol.info.typeParams })
        println("new tparams for module!? "+ name +": "+ newParams)
        postTransform(treeCopy.ModuleDef(tree, mods, name, /*tdefTrees(newParams),*/ impl))  // TODO: tparams for moduledefs?
      // add further type param refs
      case _ => postTransform(tree)
//        if (res.tpe ne null) res setType transformInfo(currentOwner, res.tpe)
//        res
    }

    object expandThisType extends TypeMap {
      def apply(tp: Type): Type = tp match {
        case ThisType(sym) if !sym.isPackageClass =>
          mapOver(tp.underlying)
        // since ThisTypes are erased to the thisType of the symbol
        // which only then contains references to type params of outers
        // do erasure here so we can subst type params -- TODO: are there other cases like this?
        case _ => mapOver(tp)
      }
    }

    // we're now in scope of the type params to, which replace type params in from, so subst them away
    class TreeTypeSymSubstituter(val from: List[Symbol], val to: List[Symbol]) extends Traverser {
      val symSubst = new SubstSymMap(from, to)
      def subst(tp: Type): Type = symSubst(expandThisType(tp))
      override def traverse(tree: Tree) {
        if (tree.tpe ne null) tree.tpe = subst(tree.tpe)
        if (tree.isDef && !affectedByFlatten(tree.symbol)) { // those affected directly will have their info transformed, here we expand so that we see the type parameters that we need to rebind the the cloned outers
          // here, we clean up and reroute references in other symbols from outer type params to their newly introduced clones in the inner class
          val sym = tree.symbol
          val info1 = subst(sym.info)
          if (info1 ne sym.info) sym.setInfo(info1)
        }
        super.traverse(tree)
      }
      override def apply[T <: Tree](tree: T): T = assert(false, "no need").asInstanceOf[T]
      override def toString() = "TreeTypeSymSubstituter("+from+","+to+")"
    }

    object treeTypePrinter extends Traverser {
      override def traverse(tree: Tree) {
        if ((tree.tpe ne null) && (tree.tpe ne NoType)) println(tree +" : "+ tree.tpe)
        super.traverse(tree)
      }
    }
    def transformSubst(from: List[Symbol], to: List[Symbol])(tree: Tree): Tree = {
      val post = postTransform(tree) // first add type arguments
      atPhase(phase.next) { // so that sym.info has the right number of type parameters
//        println("post: "+ post)
        (new TreeTypeSymSubstituter(from, to)) traverse post
//        println("substed: "+ (from, to, post))
//        treeTypePrinter(post)
        post
      }
    }
    
    def postTransform(tree: Tree): Tree =
      tree match {
        case TypeApply(fun, args) if affectedByFlatten(fun.symbol) && outerTparamRefs(fun.symbol).nonEmpty =>
          assert(!tree.isType, tree +" isType -- should be typetree by now")
          super.transform(treeCopy.TypeApply(tree, fun, args ++ targTrees(fun)))
        // add type param refs when mono became poly
        case Select(qual, name) if affectedByFlatten(tree.symbol) && outerTparamRefs(tree.symbol).nonEmpty =>
          assert(!tree.isType, tree +" isType -- should be typetree by now")
          super.transform(TypeApply(tree, targTrees(tree)))
//        case SelectFromTypeTree(qual, name) // eliminated by uncurry
//        case This(_) if affectedByFlatten(tree.symbol) && outerTparamRefs(tree.symbol).nonEmpty =>
//          val t = if(tree.tpe eq null) tree else (tree setType transformType(expandThisType(tree.tpe)))
//          println("this: "+ (tree.symbol, tree.tpe, t.tpe))
//          super.transform(t)
        case _ => // includes TypeTree, what becomes of TypeApply/Select involving types after uncurry
          val oldTp = tree.tpe
          if(tree.tpe ne null) tree setType transformType(tree.tpe)
       //   println("xf: "+ tree +" : "+ oldTp +" --> "+ tree.tpe)
          super.transform(tree)
      }
    /** The transformation method for whole compilation units */
//    override def transformUnit(unit: CompilationUnit) {
////       cunit = unit
//       atPhase(phase.next) { super.transformUnit(unit) } // so that the infos we look at are already transformed
////       cunit = null
//    }
  }

}
