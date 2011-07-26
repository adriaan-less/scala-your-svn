/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Adriaan Moors
 */

package scala.tools.nsc
package transform

import symtab._
import Flags.{ CASE => _, _ }
import scala.collection.mutable.ListBuffer

trait VirtMatcher extends ast.TreeDSL {
  import global._
  import definitions._

  def matchTranslation(localTper: analyzer.Typer, currentOwner: Symbol, tree: Tree): Tree = new MatchTranslator(localTper, currentOwner).X(tree)
  
  class MatchTranslator(val localTyper: analyzer.Typer, val currentOwner: Symbol) {
    /*
    Match(scrutinee, List(
      CaseDef(pattern, guard, body)
      CaseDef(pattern, guard, body)
      CaseDef(pattern, guard, body)
    ))

    `code`  --> code is scala syntax for the tree to be created
    'foo'   --> inside ` ... `, 'foo' interpolates the result of evaluating foo as code

    */
    import CODE._
    def matcherTycon: Type = OptionClass.typeConstructor // TODO: determined by solving M in `implicitly[MatchingStrategy[M]]`
    def tupleTypeOrUnit(tps: List[Type]): Type = if(tps.isEmpty) UnitClass.tpe else tupleType(tps)
    def matchingStrategyTycon = getMember(PredefModule, "MatchingStrategy".toTypeName)
    def Predef_implicitly = getMember(PredefModule, "implicitly".toTermName)
    def mkImplicitMatcherTree: Tree = TypeApply(REF(Predef_implicitly), List(TypeTree(appliedType(matchingStrategyTycon.typeConstructor, List(matcherTycon)))))  // implicitly[MatchingStrategy[matcherTycon]].fail
    def mkFailTree: Tree = mkImplicitMatcherTree DOT "fail".toTermName 
    def mkApplyCaseOrElse(thisCase: Tree, arg: Tree, elseCase: Tree): Tree = ((thisCase APPLY arg) DOT "orElse".toTermName) APPLY elseCase
    def mkBindTree(a: Tree, b: Tree): Tree = (a DOT ">>".toTermName) APPLY b
    def mkFunTree(arg: Symbol, body: Tree): Tree = Function(List(ValDef(arg)), body)
    def mkPatBinderTupleSel(binder: Symbol)(i: Int): Tree = REF(binder) DOT ("_"+i).toTermName // make tree that accesses the i'th component of the tuple referenced by binder
    def mkCheckTree(t: Tree): Tree = (mkImplicitMatcherTree DOT "check".toTermName) APPLY t
    def mkApplyUnapply(fun: Tree, arg: Symbol): Tree = 
      (fun match {
        case UnApply(t, _) => REF(treeInfo.methPart(t).symbol)
        case t => t
      }) APPLY REF(arg)

    def X(tree: Tree): Tree = tree match {
      case Match(scrut, cases) => 
        val xTree = cases.foldRight(mkFailTree)((currCase, accumCases) => mkApplyCaseOrElse(Xcase(scrut.tpe, currCase), scrut, accumCases))
        println("xformed: "+ xTree)
        localTyper typed xTree
      case t => t
    }

    def Xcase(scrutTpe: Type, tree: Tree): Tree = tree match {
      case CaseDef(pattern, guard, body) => 
        val scrutSym = freshSym(currentOwner, tree.pos) setInfo scrutTpe
        val extractor = threadSubstitution(Xpat(scrutSym)(pattern) ++ Xguard(guard)).foldRight(X(body)){
          case ((extractor, mkFunAndSubst), tree) => mkBindTree(extractor, mkFunAndSubst(tree))
        }

        mkFunTree(scrutSym, extractor)
    }

    var ctr = 0
    def freshSym(owner: Symbol, pos: Position) = {ctr += 1; assert(owner ne null); assert(owner ne NoSymbol); new TermSymbol(owner, pos, ("x"+ctr).toTermName)}

    type TreeXForm = Tree => Tree
    type TreeMaker = (Tree, TreeXForm => (TreeXForm, TreeXForm))

    // (o => (o(foo), newO)) :: (o => (o(foo), newO')) :: (o => (o(foo), newO'')) :: (o => (o(foo), newO'''))
    // (identity(foo), newO) :: (newO(foo), newO') :: (newO'(foo), newO'') :: (newO''(foo), newO''')
    def threadSubstitution(substTreeMakers: List[TreeMaker]): List[(Tree, TreeXForm)] = 
      substTreeMakers.foldLeft((List[(Tree, TreeXForm)](), identity[Tree](_))){ case ((accumTreeMakers, accumSubst), (extractorTree, substTreeMaker)) => 
        val (nestedTreeMaker, newSubst) = substTreeMaker(accumSubst)
        ((extractorTree, nestedTreeMaker) :: accumTreeMakers, newSubst)
      }._1.reverse

    def mkGuardTree(guard: Tree): Tree = mkCheckTree(X(guard))
    def Xguard(guard: Tree): List[TreeMaker] = 
      if (guard == EmptyTree) List()
      else List((mkGuardTree(X(guard)), { outerSubst => (nestedTree => mkFunTree(freshSym(currentOwner, nestedTree.pos) setInfo UnitClass.tpe, outerSubst(nestedTree)), outerSubst) })) // guard does not bind any variables, 

    def treeSubst(from: List[Symbol], to: List[Tree])(tree: Tree): Tree = new TreeSubstituter(from, to).transform(tree)

    def mkPatTreeMaker(patTree: Tree, patBinders: List[Symbol]): TreeMaker = 
      (patTree, { outerSubst: TreeXForm => 
                    println("patbinderstpe "+ (patBinders map (_.tpe)))
                    val binder = freshSym(currentOwner, patTree.pos) setInfo tupleTypeOrUnit(patBinders map (_.tpe))
                    def nextSubst(tree: Tree): Tree = treeSubst(patBinders, (1 to patBinders.length).toList map mkPatBinderTupleSel(binder))(outerSubst(tree))
                    ((nestedTree: Tree) => mkFunTree(binder, nextSubst(nestedTree)), nextSubst)
                })

    def Xpat(scrut: Symbol)(pattern: Tree): List[TreeMaker] = {
      depthFirstFlatten(scrut)(pattern) map {case (t, bs) => mkPatTreeMaker(t, bs)} // TODO scrut.owner, 
    }

    def depthFirstFlatten(rootSym: Symbol)(pattern: Tree): List[(Tree /* extractor call */, List[Symbol] /* the variables bound by this pattern --> must become tuple selections on extractor's result*/)] = {
      val res = new ListBuffer[(Tree, List[Symbol])]
      def traverse(currSym: Symbol, t: Tree): Unit = {
          val (nestedBinders, nestedTrees) = nestedBindersAndTrees(t)
          res += Tuple2(mkApplyUnapply(toUnapply(t), currSym), nestedBinders)
          (nestedBinders, nestedTrees).zipped foreach traverse
      }
      traverse(rootSym, pattern)
      res.toList
    }

    def nestedBindersAndTrees(tree: Tree): (List[Symbol], List[Tree]) = tree match {
      case Apply(fun, args) => (args map (_.symbol), args filterNot (_.isInstanceOf[Bind]))
      case t => (Nil, Nil)
    }

    def toUnapply(tree: Tree): Tree = tree

    /*
    scrut match { case Person(father@Person(_, fatherName), name) if fatherName == name => }
    scrut match { case Person(father, name) => father match {case Person(_, fatherName) => }}
    Person.unapply(scrut) >> ((father, name) => (Person.unapply(father) >> (_, fatherName) => check(fatherName == name) >> (_ => body)))

    a => (Person.unapply(a).>>(
      b => Person.unapply(b._1).>>(
        c => check(c._2 == b._2).>>(
          d => body))))
    */
  }
}