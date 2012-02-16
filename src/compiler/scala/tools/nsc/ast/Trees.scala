/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

import scala.reflect.internal.Flags.BYNAMEPARAM
import scala.reflect.internal.Flags.DEFAULTPARAM
import scala.reflect.internal.Flags.IMPLICIT
import scala.reflect.internal.Flags.PARAM
import scala.reflect.internal.Flags.PARAMACCESSOR
import scala.reflect.internal.Flags.PRESUPER
import scala.reflect.internal.Flags.TRAIT

trait Trees extends reflect.internal.Trees { self: Global =>

  // --- additional cases --------------------------------------------------------
  /** Only used during parsing */
  case class Parens(args: List[Tree]) extends Tree

  /** Documented definition, eliminated by analyzer */
  case class DocDef(comment: DocComment, definition: Tree)
       extends Tree {
    override def symbol: Symbol = definition.symbol
    override def symbol_=(sym: Symbol) { definition.symbol = sym }
    override def isDef = definition.isDef
    override def isTerm = definition.isTerm
    override def isType = definition.isType
  }

 /** Array selection <qualifier> . <name> only used during erasure */
  case class SelectFromArray(qualifier: Tree, name: Name, erasure: Type)
       extends TermTree with RefTree

  /** emitted by typer, eliminated by refchecks */
  case class TypeTreeWithDeferredRefCheck()(val check: () => TypeTree) extends TypTree

  /** Marks underlying reference to id as boxed.
   *  @pre: id must refer to a captured variable
   *  A reference such marked will refer to the boxed entity, no dereferencing
   *  with `.elem` is done on it.
   *  This tree node can be emitted by macros such as reify that call markBoxedReference.
   *  It is eliminated in LambdaLift, where the boxing conversion takes place.
   */
  case class ReferenceToBoxed(idt: Ident) extends TermTree

  // --- factory methods ----------------------------------------------------------

    /** Generates a template with constructor corresponding to
   *
   *  constrmods (vparams1_) ... (vparams_n) preSuper { presupers }
   *  extends superclass(args_1) ... (args_n) with mixins { self => body }
   *
   *  This gets translated to
   *
   *  extends superclass with mixins { self =>
   *    presupers' // presupers without rhs
   *    vparamss   // abstract fields corresponding to value parameters
   *    def <init>(vparamss) {
   *      presupers
   *      super.<init>(args)
   *    }
   *    body
   *  }
   */
  def Template(parents: List[Tree], self: ValDef, constrMods: Modifiers, vparamss: List[List[ValDef]], argss: List[List[Tree]], body: List[Tree], superPos: Position): Template = {
    /* Add constructor to template */

    // create parameters for <init> as synthetic trees.
    var vparamss1 =
      vparamss map (vps => vps.map { vd =>
        atPos(focusPos(vd.pos)) {
          ValDef(
            Modifiers(vd.mods.flags & (IMPLICIT | DEFAULTPARAM | BYNAMEPARAM) | PARAM | PARAMACCESSOR) withAnnotations vd.mods.annotations,
            vd.name, vd.tpt.duplicate, vd.rhs.duplicate)
        }})
    val (edefs, rest) = body span treeInfo.isEarlyDef
    val (evdefs, etdefs) = edefs partition treeInfo.isEarlyValDef
    val gvdefs = evdefs map {
      case vdef @ ValDef(mods, name, tpt, rhs) =>
        treeCopy.ValDef(
          vdef.duplicate, mods, name,
          atPos(focusPos(vdef.pos)) { TypeTree() setOriginal tpt setPos focusPos(tpt.pos) }, // atPos in case
          EmptyTree)
    }
    val lvdefs = evdefs map {
      case vdef @ ValDef(mods, name, tpt, rhs) =>
        treeCopy.ValDef(vdef, Modifiers(PRESUPER), name, tpt, rhs)
    }
    val constrs = {
      if (constrMods hasFlag TRAIT) {
        if (body forall treeInfo.isInterfaceMember) List()
        else List(
          atPos(wrappingPos(superPos, lvdefs)) (
            DefDef(NoMods, nme.MIXIN_CONSTRUCTOR, List(), List(List()), TypeTree(), Block(lvdefs, Literal(Constant())))))
      } else {
        // convert (implicit ... ) to ()(implicit ... ) if its the only parameter section
        if (vparamss1.isEmpty || !vparamss1.head.isEmpty && vparamss1.head.head.mods.isImplicit)
          vparamss1 = List() :: vparamss1;
        val superRef: Tree = atPos(superPos) {
          Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR)
        }
        val superCall = (superRef /: argss) (Apply)
        List(
          atPos(wrappingPos(superPos, lvdefs ::: argss.flatten)) (
            DefDef(constrMods, nme.CONSTRUCTOR, List(), vparamss1, TypeTree(), Block(lvdefs ::: List(superCall), Literal(Constant())))))
      }
    }
    // println("typed template, gvdefs = "+gvdefs+", parents = "+parents+", constrs = "+constrs)
    constrs foreach (ensureNonOverlapping(_, parents ::: gvdefs))
    // vparamss2 are used as field definitions for the class. remove defaults
    val vparamss2 = vparamss map (vps => vps map { vd =>
      treeCopy.ValDef(vd, vd.mods &~ DEFAULTPARAM, vd.name, vd.tpt, EmptyTree)
    })
    Template(parents, self, gvdefs ::: vparamss2.flatten ::: constrs ::: etdefs ::: rest)
  }

  /** Construct class definition with given class symbol, value parameters,
   *  supercall arguments and template body.
   *
   *  @param sym        the class symbol
   *  @param constrMods the modifiers for the class constructor, i.e. as in `class C private (...)`
   *  @param vparamss   the value parameters -- if they have symbols they
   *                    should be owned by `sym`
   *  @param argss      the supercall arguments
   *  @param body       the template statements without primary constructor
   *                    and value parameter fields.
   */
  def ClassDef(sym: Symbol, constrMods: Modifiers, vparamss: List[List[ValDef]], argss: List[List[Tree]], body: List[Tree], superPos: Position): ClassDef =
    ClassDef(sym,
      Template(sym.info.parents map TypeTree,
               if (sym.thisSym == sym || phase.erasedTypes) emptyValDef else ValDef(sym.thisSym),
               constrMods, vparamss, argss, body, superPos))

 // --- subcomponents --------------------------------------------------

  object treeInfo extends {
    val global: Trees.this.type = self
  } with TreeInfo

  lazy val treePrinter = newTreePrinter()

  // --- additional cases in operations ----------------------------------

  override protected def xtraverse(traverser: Traverser, tree: Tree): Unit = tree match {
    case Parens(ts) =>
      traverser.traverseTrees(ts)
    case DocDef(comment, definition) =>
      traverser.traverse(definition)
    case SelectFromArray(qualifier, selector, erasure) =>
      traverser.traverse(qualifier)
    case ReferenceToBoxed(idt) =>
      traverser.traverse(idt)
    case TypeTreeWithDeferredRefCheck() =>
      // (and rewrap the result? how to update the deferred check? would need to store wrapped tree instead of returning it from check)
    case _ => super.xtraverse(traverser, tree)
  }

  trait TreeCopier extends super.TreeCopierOps {
    def DocDef(tree: Tree, comment: DocComment, definition: Tree): DocDef
    def SelectFromArray(tree: Tree, qualifier: Tree, selector: Name, erasure: Type): SelectFromArray
    def ReferenceToBoxed(tree: Tree, idt: Ident): ReferenceToBoxed
    def TypeTreeWithDeferredRefCheck(tree: Tree): TypeTreeWithDeferredRefCheck
  }

  def newStrictTreeCopier: TreeCopier = new StrictTreeCopier
  def newLazyTreeCopier: TreeCopier = new LazyTreeCopier

  class StrictTreeCopier extends super.StrictTreeCopier with TreeCopier {
    def DocDef(tree: Tree, comment: DocComment, definition: Tree) =
      new DocDef(comment, definition).copyAttrs(tree)
    def SelectFromArray(tree: Tree, qualifier: Tree, selector: Name, erasure: Type) =
      new SelectFromArray(qualifier, selector, erasure).copyAttrs(tree)
    def ReferenceToBoxed(tree: Tree, idt: Ident) =
      new ReferenceToBoxed(idt).copyAttrs(tree)
    def TypeTreeWithDeferredRefCheck(tree: Tree) = tree match {
      case dc@TypeTreeWithDeferredRefCheck() => new TypeTreeWithDeferredRefCheck()(dc.check).copyAttrs(tree)
    }
  }

  class LazyTreeCopier extends super.LazyTreeCopier with TreeCopier {
    def DocDef(tree: Tree, comment: DocComment, definition: Tree) = tree match {
      case t @ DocDef(comment0, definition0)
      if (comment0 == comment) && (definition0 == definition) => t
      case _ => this.treeCopy.DocDef(tree, comment, definition)
    }
    def SelectFromArray(tree: Tree, qualifier: Tree, selector: Name, erasure: Type) = tree match {
      case t @ SelectFromArray(qualifier0, selector0, _)
      if (qualifier0 == qualifier) && (selector0 == selector) => t
      case _ => this.treeCopy.SelectFromArray(tree, qualifier, selector, erasure)
    }
  }

  abstract class Transformer {
    val treeCopy: TreeCopier = new LazyTreeCopier
    protected var currentOwner: Symbol = definitions.RootClass
    protected def currentMethod = currentOwner.enclMethod
    protected def currentClass = currentOwner.enclClass
    protected def currentPackage = currentOwner.toplevelClass.owner
    def transform(tree: Tree): Tree = tree match {
      case EmptyTree =>
        tree
      case PackageDef(pid, stats) =>
        treeCopy.PackageDef(
          tree, transform(pid).asInstanceOf[RefTree], 
          atOwner(tree.symbol.moduleClass) {
            transformStats(stats, currentOwner)
          }
        )
      case ClassDef(mods, name, tparams, impl) =>
        atOwner(tree.symbol) {
          treeCopy.ClassDef(tree, transformModifiers(mods), name,
                            transformTypeDefs(tparams), transformTemplate(impl))
        }
      case ModuleDef(mods, name, impl) =>
        atOwner(tree.symbol.moduleClass) {
          treeCopy.ModuleDef(tree, transformModifiers(mods),
                             name, transformTemplate(impl))
        }
      case ValDef(mods, name, tpt, rhs) =>
        atOwner(tree.symbol) {
          treeCopy.ValDef(tree, transformModifiers(mods),
                          name, transform(tpt), transform(rhs))
        }
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        atOwner(tree.symbol) {
          treeCopy.DefDef(tree, transformModifiers(mods), name,
                          transformTypeDefs(tparams), transformValDefss(vparamss),
                          transform(tpt), transform(rhs))
        }
      case TypeDef(mods, name, tparams, rhs) =>
        atOwner(tree.symbol) {
          treeCopy.TypeDef(tree, transformModifiers(mods), name,
                           transformTypeDefs(tparams), transform(rhs))
        }
      case LabelDef(name, params, rhs) =>
        treeCopy.LabelDef(tree, name, transformIdents(params), transform(rhs)) //bq: Martin, once, atOwner(...) works, also change `LamdaLifter.proxy'
      case Import(expr, selectors) =>
        treeCopy.Import(tree, transform(expr), selectors)
      case DocDef(comment, definition) =>
        treeCopy.DocDef(tree, comment, transform(definition))
      case Template(parents, self, body) =>
        treeCopy.Template(tree, transformTrees(parents), transformValDef(self), transformStats(body, tree.symbol))
      case Block(stats, expr) =>
        treeCopy.Block(tree, transformStats(stats, currentOwner), transform(expr))
      case CaseDef(pat, guard, body) =>
        treeCopy.CaseDef(tree, transform(pat), transform(guard), transform(body))
      case Alternative(trees) =>
        treeCopy.Alternative(tree, transformTrees(trees))
      case Star(elem) =>
        treeCopy.Star(tree, transform(elem))
      case Bind(name, body) =>
        treeCopy.Bind(tree, name, transform(body))
      case UnApply(fun, args) =>
        treeCopy.UnApply(tree, fun, transformTrees(args)) // bq: see test/.../unapplyContexts2.scala
      case ArrayValue(elemtpt, trees) =>
        treeCopy.ArrayValue(tree, transform(elemtpt), transformTrees(trees))
      case Function(vparams, body) =>
        atOwner(tree.symbol) {
          treeCopy.Function(tree, transformValDefs(vparams), transform(body))
        }
      case Assign(lhs, rhs) =>
        treeCopy.Assign(tree, transform(lhs), transform(rhs))
      case AssignOrNamedArg(lhs, rhs) =>
        treeCopy.AssignOrNamedArg(tree, transform(lhs), transform(rhs))
      case If(cond, thenp, elsep) =>
        treeCopy.If(tree, transform(cond), transform(thenp), transform(elsep))
      case Match(selector, cases) =>
        treeCopy.Match(tree, transform(selector), transformCaseDefs(cases))
      case Return(expr) =>
        treeCopy.Return(tree, transform(expr))
      case Try(block, catches, finalizer) =>
        treeCopy.Try(tree, transform(block), transformCaseDefs(catches), transform(finalizer))
      case Throw(expr) =>
        treeCopy.Throw(tree, transform(expr))
      case New(tpt) =>
        treeCopy.New(tree, transform(tpt))
      case Typed(expr, tpt) =>
        treeCopy.Typed(tree, transform(expr), transform(tpt))
      case TypeApply(fun, args) =>
        treeCopy.TypeApply(tree, transform(fun), transformTrees(args))
      case Apply(fun, args) =>
        treeCopy.Apply(tree, transform(fun), transformTrees(args))
      case ApplyDynamic(qual, args) =>
        treeCopy.ApplyDynamic(tree, transform(qual), transformTrees(args))
      case Super(qual, mix) =>
        treeCopy.Super(tree, qual, mix)
      case This(qual) =>
        treeCopy.This(tree, qual)
      case Select(qualifier, selector) =>
        treeCopy.Select(tree, transform(qualifier), selector)
      case Ident(name) =>
        treeCopy.Ident(tree, name)
      case Literal(value) =>
        treeCopy.Literal(tree, value)
      case TypeTree() =>
        treeCopy.TypeTree(tree)
      case TypeTreeWithDeferredRefCheck() =>
        treeCopy.TypeTreeWithDeferredRefCheck(tree)
      case Annotated(annot, arg) =>
        treeCopy.Annotated(tree, transform(annot), transform(arg))
      case SingletonTypeTree(ref) =>
        treeCopy.SingletonTypeTree(tree, transform(ref))
      case SelectFromTypeTree(qualifier, selector) =>
        treeCopy.SelectFromTypeTree(tree, transform(qualifier), selector)
      case CompoundTypeTree(templ) =>
        treeCopy.CompoundTypeTree(tree, transformTemplate(templ))
      case AppliedTypeTree(tpt, args) =>
        treeCopy.AppliedTypeTree(tree, transform(tpt), transformTrees(args))
      case TypeBoundsTree(lo, hi) =>
        treeCopy.TypeBoundsTree(tree, transform(lo), transform(hi)) 
      case ExistentialTypeTree(tpt, whereClauses) =>
        treeCopy.ExistentialTypeTree(tree, transform(tpt), transformTrees(whereClauses))
      case SelectFromArray(qualifier, selector, erasure) =>
        treeCopy.SelectFromArray(tree, transform(qualifier), selector, erasure)
    }

    def transformTrees(trees: List[Tree]): List[Tree] =
        trees mapConserve (transform(_))
    def transformTemplate(tree: Template): Template =
      transform(tree: Tree).asInstanceOf[Template]
    def transformTypeDefs(trees: List[TypeDef]): List[TypeDef] =
      trees mapConserve (tree => transform(tree).asInstanceOf[TypeDef])
    def transformValDef(tree: ValDef): ValDef =
      if (tree.isEmpty) tree else transform(tree).asInstanceOf[ValDef]
    def transformValDefs(trees: List[ValDef]): List[ValDef] =
      trees mapConserve (transformValDef(_))
    def transformValDefss(treess: List[List[ValDef]]): List[List[ValDef]] =
      treess mapConserve (transformValDefs(_))
    def transformCaseDefs(trees: List[CaseDef]): List[CaseDef] =
      trees mapConserve (tree => transform(tree).asInstanceOf[CaseDef])
    def transformIdents(trees: List[Ident]): List[Ident] =
      trees mapConserve (tree => transform(tree).asInstanceOf[Ident])
    def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] =
      stats mapConserve (stat =>
        if (exprOwner != currentOwner && stat.isTerm) atOwner(exprOwner)(transform(stat))
        else transform(stat)) filter (EmptyTree !=)
    def transformUnit(unit: CompilationUnit) { unit.body = transform(unit.body) }
    def transformModifiers(mods: Modifiers): Modifiers =
      Modifiers(mods.flags, mods.privateWithin, transformTrees(mods.annotations), mods.positions)

    def atOwner[A](owner: Symbol)(trans: => A): A = {
      val prevOwner = currentOwner
      currentOwner = owner
      val result = trans
      currentOwner = prevOwner
      result
    }
  }
  
  class Traverser extends super.Traverser {
    /** Compiler specific tree types are handled here: the remainder are in
     *  the library's abstract tree traverser.
     */
    override def traverse(tree: Tree): Unit = tree match {
      case AssignOrNamedArg(lhs, rhs) =>
        traverse(lhs); traverse(rhs)
      case DocDef(comment, definition) =>
        traverse(definition) 
      case Parens(ts) =>
        traverseTrees(ts)
      case TypeTreeWithDeferredRefCheck() => // TODO: should we traverse the wrapped tree?
      // (and rewrap the result? how to update the deferred check? would need to store wrapped tree instead of returning it from check)
      case _ => super.traverse(tree)
    }
    
    /** The abstract traverser is not aware of Tree.isTerm, so we override this one.
     */
    override def traverseStats(stats: List[Tree], exprOwner: Symbol) {
      stats foreach (stat =>
        if (exprOwner != currentOwner && stat.isTerm) atOwner(exprOwner)(traverse(stat))
        else traverse(stat)
      )
    }
    
    /** Leave apply available in the generic traverser to do something else.
     */
    def apply[T <: Tree](tree: T): T = { traverse(tree); tree }
  }

  private lazy val duplicator = new Transformer {
    override val treeCopy = new StrictTreeCopier
    override def transform(t: Tree) = {
      val t1 = super.transform(t)
      if ((t1 ne t) && t1.pos.isRange) t1 setPos t.pos.focus
      t1
    }
  }

  private class ShallowDuplicator(orig: Tree) extends Transformer {
    override val treeCopy = new StrictTreeCopier
    override def transform(tree: Tree) =
      if (tree eq orig) super.transform(tree)
      else tree
  }

  class TreeSubstituter(from: List[Symbol], to: List[Tree]) extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case Ident(_) =>
        def subst(from: List[Symbol], to: List[Tree]): Tree =
          if (from.isEmpty) tree
          else if (tree.symbol == from.head) to.head
          else subst(from.tail, to.tail);
        subst(from, to)
      case _ =>
        super.transform(tree)
    }
  }

  class TreeTypeSubstituter(val from: List[Symbol], val to: List[Type]) extends Traverser {
    val typeSubst = new SubstTypeMap(from, to)
    override def traverse(tree: Tree) {
      if (tree.tpe ne null) tree.tpe = typeSubst(tree.tpe)
      if (tree.isDef) {
        val sym = tree.symbol
        val info1 = typeSubst(sym.info)
        if (info1 ne sym.info) sym.setInfo(info1)
      }
      super.traverse(tree)
    }
    override def apply[T <: Tree](tree: T): T = super.apply(tree.duplicate)
    override def toString() = "TreeTypeSubstituter("+from+","+to+")"
  }

  lazy val EmptyTreeTypeSubstituter = new TreeTypeSubstituter(List(), List())

  class TreeSymSubstTraverser(val from: List[Symbol], val to: List[Symbol]) extends Traverser {
    val subst = new SubstSymMap(from, to)
    override def traverse(tree: Tree) {
      if (tree.tpe ne null) tree.tpe = subst(tree.tpe)
      if (tree.isDef) {
        val sym = tree.symbol
        val info1 = subst(sym.info)
        if (info1 ne sym.info) sym.setInfo(info1)
      }
      super.traverse(tree)
    }
    override def apply[T <: Tree](tree: T): T = super.apply(tree.duplicate)
    override def toString() = "TreeSymSubstTraverser("+from+","+to+")"
  }

  /** Substitute symbols in 'from' with symbols in 'to'. Returns a new
   *  tree using the new symbols and whose Ident and Select nodes are
   *  name-consistent with the new symbols. 
   */
  class TreeSymSubstituter(from: List[Symbol], to: List[Symbol]) extends Transformer {
    val symSubst = new SubstSymMap(from, to)
    override def transform(tree: Tree): Tree = {
      def subst(from: List[Symbol], to: List[Symbol]) {
        if (!from.isEmpty)
          if (tree.symbol == from.head) tree setSymbol to.head
          else subst(from.tail, to.tail)
      }

      if (tree.tpe ne null) tree.tpe = symSubst(tree.tpe)
      if (tree.hasSymbol) {
        subst(from, to)
        tree match {
          case Ident(name0) if tree.symbol != NoSymbol =>
            treeCopy.Ident(tree, tree.symbol.name)
          case Select(qual, name0) =>
            treeCopy.Select(tree, transform(qual), tree.symbol.name)
          case _ =>
            super.transform(tree)
        }
      } else
        super.transform(tree)
    }
    def TypeTreeWithDeferredRefCheck(tree: Tree) = tree match {
      case t @ TypeTreeWithDeferredRefCheck() => t
      case _ => this.treeCopy.TypeTreeWithDeferredRefCheck(tree)
    }
  }

  class Transformer extends super.Transformer {
    def transformUnit(unit: CompilationUnit) {
      try unit.body = transform(unit.body)
      catch {
        case ex: Exception =>
          println(supplementErrorMessage("unhandled exception while transforming "+unit))
          throw ex
      }
    }
  }

  override protected def xtransform(transformer: super.Transformer, tree: Tree): Tree = tree match {
    case DocDef(comment, definition) =>
      transformer.treeCopy.DocDef(tree, comment, transformer.transform(definition))
    case SelectFromArray(qualifier, selector, erasure) =>
      transformer.treeCopy.SelectFromArray(
        tree, transformer.transform(qualifier), selector, erasure)
    case ReferenceToBoxed(idt) =>
      transformer.treeCopy.ReferenceToBoxed(
        tree, transformer.transform(idt) match { case idt1: Ident => idt1 })
    case TypeTreeWithDeferredRefCheck() =>
      transformer.treeCopy.TypeTreeWithDeferredRefCheck(tree)
  }

  object resetPos extends Traverser {
    override def traverse(t: Tree) {
      if (t != EmptyTree) t.setPos(NoPosition)
      super.traverse(t)
    }
  }

  /** resets symbol and tpe fields in a tree, @see ResetAttrs
   */
//  def resetAllAttrs[A<:Tree](x:A): A = { new ResetAttrsTraverser().traverse(x); x }
//  def resetLocalAttrs[A<:Tree](x:A): A = { new ResetLocalAttrsTraverser().traverse(x); x }

  def resetAllAttrs[A<:Tree](x:A): A = new ResetAttrs(false).transform(x)
  def resetLocalAttrs[A<:Tree](x:A): A = new ResetAttrs(true).transform(x)

  /** A transformer which resets symbol and tpe fields of all nodes in a given tree,
   *  with special treatment of:
   *    TypeTree nodes: are replaced by their original if it exists, otherwise tpe field is reset
   *                    to empty if it started out empty or refers to local symbols (which are erased).
   *    TypeApply nodes: are deleted if type arguments end up reverted to empty
   *    This(pkg) nodes where pkg is a package: these are kept.
   *
   *  (bq:) This transformer has mutable state and should be discarded after use
   */
  private class ResetAttrs(localOnly: Boolean) {
    val debug = settings.debug.value
    val trace = scala.tools.nsc.util.trace when debug

    val locals = util.HashSet[Symbol](8)
    val orderedLocals = collection.mutable.ListBuffer[Symbol]()
    def registerLocal(sym: Symbol) {
      if (sym != null && sym != NoSymbol) {
        if (debug && !(locals contains sym)) orderedLocals append sym
        locals addEntry sym
      }
    }

    class MarkLocals extends self.Traverser {
      def markLocal(tree: Tree) {
        if (tree.symbol != null && tree.symbol != NoSymbol) {
          val sym = tree.symbol
          registerLocal(sym)
          registerLocal(sym.sourceModule)
          registerLocal(sym.moduleClass)
        }
      }

      override def traverse(tree: Tree) = {
        tree match {
         case _: DefTree | Function(_, _) | Template(_, _, _) =>
           markLocal(tree)
         case _ if tree.symbol.isInstanceOf[FreeVar] =>
           markLocal(tree)
         case _ =>
           ;
        }

        super.traverse(tree)
      }
    }

    class Transformer extends self.Transformer {
      override def transform(tree: Tree): Tree = super.transform {
        tree match {
          case tpt: TypeTree =>
            if (tpt.original != null) {
              transform(tpt.original)
            } else {
              if (tpt.tpe != null && (tpt.wasEmpty || (tpt.tpe exists (tp => locals contains tp.typeSymbol))))
                tpt.tpe = null
              tree
            }
          case TypeApply(fn, args) if args map transform exists (_.isEmpty) =>
            transform(fn)
          case This(_) if tree.symbol != null && tree.symbol.isPackageClass =>
            tree
          case EmptyTree =>
            tree
          case _ =>
            if (tree.hasSymbol && (!localOnly || (locals contains tree.symbol)))
              tree.symbol = NoSymbol
            tree.tpe = null
            tree
        }
      }
    }

    def transform[T <: Tree](x: T): T = {
      new MarkLocals().traverse(x)

      if (debug) {
        assert(locals.size == orderedLocals.size)
        val eoln = System.getProperty("line.separator")
        val msg = orderedLocals.toList filter {_ != NoSymbol} map {"  " + _} mkString eoln
        trace("locals (%d total): %n".format(orderedLocals.size))(msg)
      }

      val x1 = new Transformer().transform(x)
      assert(x.getClass isInstance x1)
      x1.asInstanceOf[T]
    }
  }

  /* New pattern matching cases:

   case Parens(expr)                                               (only used during parsing)
   case DocDef(comment, defn) =>                                   (eliminated by typer)
   case TypeTreeWithDeferredRefCheck() =>                          (created and eliminated by typer)
   case SelectFromArray(_, _, _) =>                                (created and eliminated by erasure)

  */

 }
