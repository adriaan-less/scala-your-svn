/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Adriaan Moors
 */

package scala.tools.nsc
package transform

import symtab._
import Flags.{ CASE => _, _ }
import scala.collection.mutable.ListBuffer

/** Translate pattern matching into method calls. Turn all patterns into unapplies


    scrut match { case Person(father@Person(_, fatherName), name) if fatherName == name => }
    scrut match { case Person(father, name) => father match {case Person(_, fatherName) => }}
    Person.unapply(scrut) >> ((father, name) => (Person.unapply(father) >> (_, fatherName) => check(fatherName == name) >> (_ => body)))

    (a => (Person.unapply(a).>>(
      b => Person.unapply(b._1).>>(
        c => check(c._2 == b._2).>>(
          d => body)))))(scrut)

  */
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
    type TreeXForm = Tree => Tree
    type TreeMaker = (Tree, TreeXForm => (TreeXForm, TreeXForm))

    /** Implement a pattern match by turning its cases (including the implicit failure case) 
      * into the corresponding the (monadic) extractors and combining them with the `orElse` combinator.
      */
    def X(tree: Tree): Tree = tree match {
      case Match(scrut, cases) => 
        val scrutSym = freshSym(currentOwner, tree.pos) setInfo scrut.tpe
        val xTree = // ( x => Xcase1.orElse(Xcase2).....orElse(fail) )(scrut)
          mkApply(mkFunTree(scrutSym,
            (cases :\ mkFailTree)((currCase, accumCases) => 
              mkOrElse(Xcase(scrutSym, currCase), accumCases))),
          scrut)
        println("xformed: "+ xTree)
        localTyper typed xTree
      case t => t
    }

    def Xcase(scrutSym: Symbol, tree: Tree): Tree = {
      // (o => (o(foo), newO)) :: (o => (o(foo), newO')) :: (o => (o(foo), newO'')) :: (o => (o(foo), newO'''))
      // (identity(foo), newO) :: (newO(foo), newO') :: (newO'(foo), newO'') :: (newO''(foo), newO''')
      def threadSubstitution(substTreeMakers: List[TreeMaker]): List[(Tree, TreeXForm)] = 
        substTreeMakers.foldLeft((List[(Tree, TreeXForm)](), identity[Tree](_))){ case ((accumTreeMakers, accumSubst), (extractorTree, substTreeMaker)) => 
          val (nestedTreeMaker, newSubst) = substTreeMaker(accumSubst)
          ((extractorTree, nestedTreeMaker) :: accumTreeMakers, newSubst)
        }._1.reverse

      tree match {
        case CaseDef(pattern, guard, body) => 
          threadSubstitution(Xpat(scrutSym)(pattern) ++ Xguard(guard)).foldRight(mkSuccessTree(X(body))){
            case ((extractor, mkFunAndSubst), tree) => mkBindTree(extractor, mkFunAndSubst(tree))
          }
      }
    }

    def mkGuardTree(guard: Tree): Tree = mkCheckTree(X(guard))
    def Xguard(guard: Tree): List[TreeMaker] = 
      if (guard == EmptyTree) List()
      else List(
        (mkGuardTree(X(guard)), 
          { outerSubst => 
            (nestedTree => mkFunTree(freshSym(currentOwner, nestedTree.pos) setInfo UnitClass.tpe, 
             outerSubst(nestedTree)), outerSubst) // guard does not bind any variables, so next subst is the current one
          }))

    def Xpat(scrutSym: Symbol)(pattern: Tree): List[TreeMaker] = {
      /** `patTree` is the extractor call
        * `patBinders` are the variables bound by this pattern --> must become tuple selections on extractor's result
        */
      def mkPatTreeMaker(patTree: Tree, patBinders: List[Symbol]): TreeMaker = 
        (patTree, 
          { outerSubst: TreeXForm => 
              println("patbinderstpe "+ (patBinders map (_.tpe)))
              val binder = freshSym(currentOwner, patTree.pos) setInfo tupleTypeOrUnit(patBinders map (_.tpe))
              def nextSubst(tree: Tree): Tree = new TreeSubstituter(patBinders, (1 to patBinders.length).toList map mkPatBinderTupleSel(binder)).transform(outerSubst(tree))
              ((nestedTree: Tree) => mkFunTree(binder, 
               nextSubst(nestedTree)), nextSubst)
          })

      val res = new ListBuffer[TreeMaker]
      def traverseDepthFirst(currSym: Symbol, t: Tree): Unit = {
          val (nestedBinders, nestedTrees) = nestedBindersAndTrees(t)
          res += mkPatTreeMaker(mkApplyUnapply(toUnapply(t), currSym), nestedBinders)  // TODO scrutSym.owner, 
          (nestedBinders, nestedTrees).zipped foreach traverseDepthFirst
      }
      traverseDepthFirst(scrutSym, pattern)
      res.toList
    }


// tree exegesis, rephrasing everything in terms of extractors 
    /** A conservative approximation of which patterns do not discern anything.
      */
    def isWildcardPattern(pat: Tree): Boolean = pat match {
      case Bind(_, body)        => isWildcardPattern(body)
      case Ident(nme.WILDCARD)  => true
      case x: Ident             => treeInfo.isVarPattern(x)
      case Alternative(ps)      => ps forall isWildcardPattern
      case EmptyTree            => true
      case _                    => false
    }

    /** Decompose the pattern in `tree`, returning the symbols it binds at the top-level, and its pattern sub-trees (if any)
      */
    def nestedBindersAndTrees(tree: Tree): (List[Symbol], List[Tree]) = tree match {
      case Apply(fun, args) => (args map (_.symbol), args filterNot isWildcardPattern)
      case UnApply(unfun, args) => 
        val binderTypes = analyzer.unapplyTypeList(unfun.symbol, unfun.tpe)
        println("unapp: "+ (unfun, args, args map (_.symbol), binderTypes zip (args map (_.symbol.info))))
        (args map (_.symbol), args filterNot isWildcardPattern)
      case t => (Nil, Nil)
    }

    def toUnapply(tree: Tree): Tree = tree

// code gen
    var ctr = 0
    def freshSym(owner: Symbol, pos: Position) = {ctr += 1; assert(owner ne null); assert(owner ne NoSymbol); new TermSymbol(owner, pos, ("x"+ctr).toTermName)}

    def matcherTycon: Type = OptionClass.typeConstructor // TODO: determined by solving M in `implicitly[MatchingStrategy[M]]`
    def tupleTypeOrUnit(tps: List[Type]): Type = if(tps.isEmpty) UnitClass.tpe else tupleType(tps)
    def matchingStrategyTycon = getMember(PredefModule, "MatchingStrategy".toTypeName)
    def Predef_implicitly = getMember(PredefModule, "implicitly".toTermName)

    import CODE._
    // TODO: lift out this call and cache the result
    def mkImplicitMatcherTree: Tree = TypeApply(REF(Predef_implicitly), List(TypeTree(appliedType(matchingStrategyTycon.typeConstructor, List(matcherTycon)))))  // implicitly[MatchingStrategy[matcherTycon]].fail
    def mkFailTree: Tree = mkImplicitMatcherTree DOT "fail".toTermName 
    def mkSuccessTree(res: Tree): Tree = (mkImplicitMatcherTree DOT "success".toTermName) APPLY res
    def mkOrElse(thisCase: Tree, elseCase: Tree): Tree = (thisCase DOT "orElse".toTermName) APPLY elseCase
    def mkApply(fun: Tree, arg: Tree): Tree = fun APPLY arg
    def mkBindTree(a: Tree, b: Tree): Tree = (a DOT "flatMap".toTermName) APPLY b
    def mkFunTree(arg: Symbol, body: Tree): Tree = Function(List(ValDef(arg)), body)
    def mkPatBinderTupleSel(binder: Symbol)(i: Int): Tree = REF(binder) DOT ("_"+i).toTermName // make tree that accesses the i'th component of the tuple referenced by binder
    def mkCheckTree(t: Tree): Tree = (mkImplicitMatcherTree DOT "check".toTermName) APPLY t
    def mkApplyUnapply(fun: Tree, arg: Symbol): Tree = 
      (fun match {
        case UnApply(t, _) => REF(treeInfo.methPart(t).symbol)
        case t => t
      }) APPLY REF(arg)
  }
}