/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Adriaan Moors
 */

package scala.tools.nsc
package transform

import symtab._
import Flags.{ CASE => _, _ }
import scala.collection.mutable.ListBuffer

/** Translate pattern matching into method calls (these methods are grouped as a zero-plus monad).
  *
  * For each case, express all patterns as extractor calls, guards as 0-ary extractors, and sequence them using `flatMap` 
  * (lifting the body of the case into the monad using `success`).
  *
  * Cases are combined into a pattern match using the `orElse` combinator (the implicit failure case is expressed using the monad's `fail`).
  *
  * The monad `M` in which the pattern match is interpreted is determined by solving `implicitly[MatchingStrategy[M]]` for M.
  * Predef provides the default, `Option`:
  
      implicit object OptionMatching extends MatchingStrategy[Option] {
        def fail: Option[Nothing] = None
        def success[T](x: T): Option[T] = Some(x)
      }

  * Example translation: TODO
  
    scrut match { case Person(father@Person(_, fatherName), name) if fatherName == name => }
    scrut match { case Person(father, name) => father match {case Person(_, fatherName) => }}
    Person.unapply(scrut) >> ((father, name) => (Person.unapply(father) >> (_, fatherName) => check(fatherName == name) >> (_ => body)))

    (a => (Person.unapply(a).>>(
      b => Person.unapply(b._1).>>(
        c => check(c._2 == b._2).>>(
          d => body)))))(scrut)


  * TODO: recover exhaustivity and unreachability checking using a variation on the type-safe builder pattern
  */
trait VirtMatcher extends ast.TreeDSL {
  import global._
  import definitions._

  def matchTranslation(localTper: analyzer.Typer, currentOwner: Symbol, tree: Tree): Tree = new MatchTranslator(localTper, currentOwner).X(tree)
  
  class MatchTranslator(val localTyper: analyzer.Typer, val currentOwner: Symbol) {
    assert(currentOwner ne NoSymbol)
    assert(currentOwner.owner ne NoSymbol)
    
    /*
    Match(scrutinee, List(
      CaseDef(pattern, guard, body)
      CaseDef(pattern, guard, body)
      CaseDef(pattern, guard, body)
    ))

    `code`  --> code is scala syntax for the tree to be created
    'foo'   --> inside ` ... `, 'foo' interpolates the result of evaluating foo as code

    */

    /** Implement a pattern match by turning its cases (including the implicit failure case) 
      * into the corresponding (monadic) extractors, and combining them with the `orElse` combinator.
      *
      * For `scrutinee match { case1 ... caseN }`, the resulting tree has the shape 
      * `( x => Xcase1(x).orElse(Xcase2(x)).....orElse(fail) )(scrutinee)`.
      */
    def X(tree: Tree): Tree = tree match {
      case Match(scrut, cases) => 
        val scrutSym = freshSym(currentOwner, tree.pos) setInfo scrut.tpe
        val xTree =
          mkApply(mkFun(scrutSym, ((cases map Xcase(scrutSym)) ++ List(mkFail)) reduceLeft mkOrElse), scrut)
        println("xformed: "+ xTree)
        localTyper.context.implicitsEnabled = true
        atPhase(currentRun.typerPhase) {
          localTyper typed xTree
        }
      case t => t
    }

    type TreeXForm = Tree => Tree
    type TreeMaker = (Tree, TreeXForm => (TreeXForm, TreeXForm))

    /**  The translation of `pat if guard => body` has two aspects: 
      *     1) the substitution due to the variables bound by patterns
      *     2) the combination of the extractor calls using `flatMap`.
      *
      * 2) is easy -- it looks like: `Xpat_1.flatMap(Xpat_2).....flatMap(Xpat_N).flatMap(Xguard).flatMap(success(Xbody))`
      * 1) is tricky because Xpat_i determines the shape of Xpat_i+1: 
      *    zoom in on `Xpat_1.flatMap(Xpat_2)` for example: `Xpat_1.flatMap(Xpat_2)` the result type of Xpat_i's extractor call determines the variables in the 
      */
    def Xcase(scrutSym: Symbol)(tree: Tree): Tree = {
      // (o => (o(foo), newO)) :: (o => (o(foo), newO')) :: (o => (o(foo), newO'')) :: (o => (o(foo), newO'''))
      // (identity(foo), newO) :: (newO(foo), newO') :: (newO'(foo), newO'') :: (newO''(foo), newO''')
      def threadSubstitution(substTreeMakers: List[TreeMaker]): List[(Tree, TreeXForm)] = 
        substTreeMakers.foldLeft((List[(Tree, TreeXForm)](), identity[Tree](_))){ case ((accumTreeMakers, accumSubst), (extractorTree, substTreeMaker)) => 
          val (nestedTreeMaker, newSubst) = substTreeMaker(accumSubst)
          ((extractorTree, nestedTreeMaker) :: accumTreeMakers, newSubst)
        }._1.reverse

      tree match {
        case CaseDef(pattern, guard, body) => 
          threadSubstitution(Xpat(scrutSym)(pattern) ++ Xguard(guard)).foldRight(mkSuccess(X(body))){ // TODO: if we want to support a generalisation of Kotlin's patmat continue, must not hard-wire lifting into the monad (mkSuccess), so that user can generate failure when needed -- use implicit conversion to lift into monad on-demand
            case ((extractor, mkFunAndSubst), tree) => mkFlatMap(extractor, mkFunAndSubst(tree))
          }
      }
    }

    def Xpat(scrutSym: Symbol)(pattern: Tree): List[TreeMaker] = {
      /** `patTree` is the extractor call
        * `patBinders` are the variables bound by this pattern --> must become tuple selections on extractor's result
        */
      def mkPatTreeMaker(patTree: Tree, subPatBinders: List[Symbol], subPatTypes: List[Type]): TreeMaker = {
        (patTree, 
          { outerSubst: TreeXForm =>
              val binder = freshSym(currentOwner, patTree.pos) setInfo tupleTypeOrUnit(subPatTypes)
              val subst = new TreeSubstituter(subPatBinders, (1 to subPatBinders.length).map(mkPatBinderTupleSel(binder)).toList)
              println("pat next subst: "+ subst)
              def nextSubst(tree: Tree): Tree = subst.transform(outerSubst(tree))
              (nestedTree => mkFun(binder, nextSubst(nestedTree)), nextSubst)
          })
      }

      val res = new ListBuffer[TreeMaker]
      def traverseDepthFirst(currSym: Symbol, t: Tree): Unit = if (!isWildcardPattern(t)) { // skip wildcard trees -- no point in checking them
          val (subPatBinders, subPatTypes, subPatTrees) = nestedBindersAndTrees(t)
          res += mkPatTreeMaker(mkApplyUnapply(toUnapply(t), currSym), subPatBinders, subPatTypes)
          (subPatBinders, subPatTrees).zipped foreach traverseDepthFirst
      }
      traverseDepthFirst(scrutSym, pattern)
      res.toList
    }

    def Xguard(guard: Tree): List[TreeMaker] = {
      def mkGuardTree(guard: Tree): Tree = mkCheck(X(guard))

      if (guard == EmptyTree) List()
      else List(
        (mkGuardTree(X(guard)), 
          { outerSubst => 
            val binder = freshSym(currentOwner, guard.pos) setInfo UnitClass.tpe
            (nestedTree => mkFun(binder, outerSubst(nestedTree)), outerSubst) // guard does not bind any variables, so next subst is the current one
          }))
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

    /** Decompose the pattern in `tree`, of shape C(p_1, ..., p_N), into a list of N symbols, and a list of its N sub-trees
      * The list of N symbols contains symbols for every bound name as well as the un-named sub-patterns (fresh symbols are generated here for these)
      */
    def nestedBindersAndTrees(tree: Tree): (List[Symbol], List[Type], List[Tree]) = tree match {
      case Apply(fun, args) => (args map (_.symbol), args map (_.symbol.tpe), args)
      case UnApply(unfun, args) => 
        val binderTypes = analyzer.unapplyTypeList(unfun.symbol, unfun.tpe)
        val fixedArgs = (args, binderTypes).zipped map {(a, tp) => if(a.symbol ne null) a.symbol else (freshSym(currentOwner, tree.pos, "p") setInfo tp)}
        (fixedArgs, binderTypes, args)
      case t => (Nil, Nil, Nil)
    }

    def toUnapply(tree: Tree): Tree = tree

// code gen

    var ctr = 0
    def freshSym(owner: Symbol, pos: Position, prefix: String = "x") = {ctr += 1; assert(owner ne null); assert(owner ne NoSymbol); new TermSymbol(owner, pos, (prefix+ctr).toTermName)}

    def matcherTycon: Type = OptionClass.typeConstructor // TODO: determined by solving M in `implicitly[MatchingStrategy[M]]`
    def tupleTypeOrUnit(tps: List[Type]): Type = if(tps.isEmpty) UnitClass.tpe else tupleType(tps)
    def matchingStrategyTycon = getMember(PredefModule, "MatchingStrategy".toTypeName)
    def Predef_implicitly = getMember(PredefModule, "implicitly".toTermName)

    import CODE._
    // TODO: lift out this call and cache the result
    def ref(s: Symbol) = {assert(s.owner ne NoSymbol); REF(s)}
    def mkImplicitMatcher: Tree = TypeApply(ref(Predef_implicitly), List(TypeTree(appliedType(matchingStrategyTycon.typeConstructor, List(matcherTycon)))))  // implicitly[MatchingStrategy[matcherTycon]].fail
    def mkFail: Tree = mkImplicitMatcher DOT "fail".toTermName 
    def mkSuccess(res: Tree): Tree = (mkImplicitMatcher DOT "success".toTermName) APPLY res
    def mkOrElse(thisCase: Tree, elseCase: Tree): Tree = (thisCase DOT "orElse".toTermName) APPLY elseCase
    def mkApply(fun: Tree, arg: Tree): Tree = fun APPLY arg
    def mkFlatMap(a: Tree, b: Tree): Tree = (a DOT "flatMap".toTermName) APPLY b
    def mkFun(arg: Symbol, body: Tree): Tree = Function(List(ValDef(arg)), body)
    def mkPatBinderTupleSel(binder: Symbol)(i: Int): Tree = ref(binder) DOT ("_"+i).toTermName // make tree that accesses the i'th component of the tuple referenced by binder
    def mkCheck(t: Tree): Tree = (mkImplicitMatcher DOT "check".toTermName) APPLY t
    def mkApplyUnapply(fun: Tree, arg: Symbol): Tree = 
      (fun match {
        case UnApply(t, _) => ref(treeInfo.methPart(t).symbol)
        case t => t
      }) APPLY ref(arg)
  }
}