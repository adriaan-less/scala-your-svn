/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Adriaan Moors
 */

package scala.tools.nsc
package typechecker

import symtab._
import Flags.{ CASE => _, _ }
import scala.collection.mutable.ListBuffer

/** Translate pattern matching into method calls (these methods form a zero-plus monad), similar in spirit to how for-comprehensions are compiled.
  *
  * For each case, express all patterns as extractor calls, guards as 0-ary extractors, and sequence them using `flatMap` 
  * (lifting the body of the case into the monad using `success`).
  *
  * Cases are combined into a pattern match using the `orElse` combinator (the implicit failure case is expressed using the monad's `fail`).
  *
  * The monad `M` in which the pattern match is interpreted is determined by solving `implicitly[MatchingStrategy[M]]` for M.
  * Predef provides the default, `Option`:
  
      implicit object OptionMatching extends MatchingStrategy[Option] {
        def fail: Option[Nothing] = None // TODO: throw new MatchError?
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


  * (longer-term) TODO: 
  *  - recover exhaustivity and unreachability checking using a variation on the type-safe builder pattern
  *  - recover GADT typing by locally inserting implicit witnesses to type equalities derived from the current case, and considering these witnesses during subtyping (?)
  */
trait PatMatVirtualiser extends ast.TreeDSL { self: Analyzer =>
  import global._
  import definitions._

  def typedMatch(typer: Typer, tree: Tree, selector: Tree, cases: List[CaseDef], mode: Int, pt: Type): Tree = {
    import typer._

    val selector1 = typed(selector, EXPRmode | BYVALmode, WildcardType) // TODO: handle empty selector (just remove outer apply node from xTree?)
    val xTree = new MatchTranslator(typer).X(treeCopy.Match(tree, selector1, typedCases(tree, cases, selector1.tpe.widen, pt)))
    println("xformed patmat: "+ xTree)
    typed(xTree, mode, pt)
  }
  
  private class MatchTranslator(typer: Typer) {
    import typer._
    val currentOwner: Symbol = context.owner
    
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
        mkApply(mkFun(scrutSym, ((cases map Xcase(scrutSym)) ++ List(mkFail)) reduceLeft mkOrElse), scrut)
      case t => t
    }

    type TreeXForm = Tree => Tree
    type ProtoTreeMaker = (List[Tree], TreeXForm => (TreeXForm /* wrap a Fun */, TreeXForm /* subst variables to tuple sel on variable bound by that Fun */))

    object TreeMaker {
      def apply(trees: List[Tree], mkFunAndSubst0: TreeXForm): TreeMaker =
        if (trees isEmpty) new NoTreeMaker{def mkFunAndSubst(next: Tree) = mkFunAndSubst0(next)}
        else if (trees.length == 1) new SingleTreeMaker(trees.head){def mkFunAndSubst(next: Tree) = mkFunAndSubst0(next)}
        else new AlternativeTreeMaker(trees){def mkFunAndSubst(next: Tree) = mkFunAndSubst0(next)}
    }
    abstract class TreeMaker {
      // wrap a Fun (with binder x) around the next tree and do aggregated substitution (which
      // replaces old pattern bindings by the appropriate tuple element selection on the new binders,
      // that is, `x`, if it was bound by the immediately enclosing pattern)
      def mkFunAndSubst(next: Tree): Tree

      // build Tree that chains `next` after the current extractor
      def flatMap(next: Tree): Tree
    }

    abstract class NoTreeMaker extends TreeMaker {
      def flatMap(tree: Tree) = mkFunAndSubst(tree) // doesn't make a fun, only does substitution
    }

    abstract class SingleTreeMaker(extractor: Tree) extends TreeMaker {
      def flatMap(tree: Tree) = mkFlatMap(extractor, mkFunAndSubst(tree))
    }

    abstract class AlternativeTreeMaker(alts: List[Tree]) extends TreeMaker {
      def flatMap(tree: Tree) = mkOr(alts, mkFunAndSubst(tree))
    }

    /**  The translation of `pat if guard => body` has two aspects: 
      *     1) the substitution due to the variables bound by patterns
      *     2) the combination of the extractor calls using `flatMap`.
      *
      * 2) is easy -- it looks like: `Xpat_1.flatMap(Xpat_2....flatMap(Xpat_N.flatMap(Xguard.flatMap((x_i) => success(Xbody(x_i)))))...)`
      *     this must be right-leaning tree, as can be seen intuitively by considering the scope of bound variables: 
      *     variables bound by pat_1 must be visible from the function inside the left-most flatMap right up to Xbody all the way on the right
      * 1) is tricky because Xpat_i determines the shape of Xpat_i+1: 
      *    zoom in on `Xpat_1.flatMap(Xpat_2)` for example -- it actually looks more like:
      *      `Xpat_1(x_scrut).flatMap((x_1) => {y_i -> x_1._i}Xpat_2)` 
      *
      *    `x_1` references the result (inside the monad) of the extractor corresponding to `pat_1`, 
      *    this result is a tuple consisting of the values for the constructor arguments that Xpat_1
      *    has extracted from the object pointed to by `x_scrut`. The `y_i` are the symbols bound by `pat_1` (in order) 
      *    in the scope of the remainder of the pattern, and they must thus be replaced by tuple selection calls on `x_1` (corresponding by position).
      *
      *    Thus, the result type of `Xpat_i`'s extractor must conform to `M[(T_1,..., T_n)]`.
      * 
      *    Operationally, phase 1) is a foldLeft, since we must consider the depth-first-flattening of
      *    the transformed patterns from left to right. For every pattern ast node, it produces a transformed ast and 
      *    a function that will take care of binding and substitution of the next ast (to the right).
      *    
      *    `threadSubstitution` takes these pairs and accumulates the substitution from left to right, so that the rightmost substitution (a function from Tree to Tree)
      *    will substitute each bound pattern variable in the whole case.
      */
    def Xcase(scrutSym: Symbol)(tree: Tree): Tree = {
      // (o => (o(foo), newO)) :: (o => (o(foo), newO')) :: (o => (o(foo), newO'')) :: (o => (o(foo), newO'''))
      // (identity(foo), newO) :: (newO(foo), newO') :: (newO'(foo), newO'') :: (newO''(foo), newO''')
      def threadSubstitution(protoTreeMakers: List[ProtoTreeMaker]): List[TreeMaker] = 
        protoTreeMakers.foldLeft((List[TreeMaker](), identity[Tree](_))){ 
          case ((accumTreeMakers, accumSubst), (extractors, substTreeMaker)) => 
            val (nestedTreeMaker, newSubst) = substTreeMaker(accumSubst)
            (TreeMaker(extractors, nestedTreeMaker) :: accumTreeMakers, newSubst)
        }._1.reverse // ._2 is the state we accumulated while traversing the original list to build the result in _.1

      tree match {
        case CaseDef(pattern, guard, body) => 
          threadSubstitution(Xpat(scrutSym)(pattern) ++ Xguard(guard)).foldRight(mkSuccess(X(body)))(_.flatMap(_)) 
          // TODO: if we want to support a generalisation of Kotlin's patmat continue, must not hard-wire lifting into the monad (mkSuccess), so that user can generate failure when needed -- use implicit conversion to lift into monad on-demand
      }
    }

    /** TODO: handle Alternative patterns
      * A pattern alternative p1 | ... | pn consists of a number of alternative patterns pi.
      * All alternative patterns are type checked with the expected type of the pattern.
      * They may no bind variables other than wildcards. The alternative pattern matches a
      * value v if at least one its alternatives matches v.
      *
      * Since alternatives may not introduce bindings, can we just combine the alternatives into 
      * something which implements flatMap by delegating the flatMap to each alternative in order until it succeeds?
      * I.e., alts.reduceLeft(mkOr) where
      *   class Option[T] { self =>
            def or(x: Option[T]) = new Option[T] {
              def isEmpty: Boolean
              def get: A // hrm, this is where Option's not-quite-being-a-monad gets us into trouble -- what you get depends on what you want (i.e., the function you flatMap) -- should we expand it inline? (resulting expression may get pretty big (?))
              def flatMap[U](f: T => Option[U]): Option[U] = self.flatMap(f) orElse other.flatMap(f)
            }
          }
      * Probably allow lists of tree's in a treemaker and then:
      * case ((extractors, mkFunAndSubst), tree) => extractors map (mkFlatMap(_, mkFunAndSubst(tree))) reduceLeft mkOr






      */
    def Xpat(scrutSym: Symbol)(pattern: Tree): List[ProtoTreeMaker] = {
      /** `patTree` is the extractor call
        * `patBinders` are the variables bound by this pattern in the following patterns --> must become tuple selections on extractor's result
        */
      def patProtoTreeMaker(patTree: Tree, patBinders: List[Symbol]): ProtoTreeMaker = {
        (List(patTree), 
          if(patBinders isEmpty)           
            { outerSubst: TreeXForm =>
                val binder = freshSym(currentOwner, patTree.pos) setInfo UnitClass.tpe
                (nestedTree => mkFun(binder, outerSubst(nestedTree)), outerSubst)
            }
          else
            { outerSubst: TreeXForm =>
                val binder = freshSym(currentOwner, patTree.pos) setInfo tupleType(patBinders map (_.info))
                val theSubst = mkTypedSubst(patBinders, (1 to patBinders.length).map(mkPatBinderTupleSel(binder)).toList)
                def nextSubst(tree: Tree): Tree = outerSubst(theSubst.transform(tree))
                (nestedTree => mkFun(binder, nextSubst(nestedTree)), nextSubst)
            })
      }

      // since alternatives may not bind variables (except wildcards), only the trees in `alts`' ProtoTreeMakers matter
      def altsProtoTreeMaker(alts: List[ProtoTreeMaker], patBinder: Symbol): ProtoTreeMaker = singleBinderMultiProtoTreeMaker(alts.flatMap(_._1), patBinder, UnitClass.tpe) // alternative pattern

      def singleBinderProtoTreeMaker(patTree: Tree, patBinder: Symbol): ProtoTreeMaker = singleBinderMultiProtoTreeMaker(List(patTree), patBinder, patBinder.info)

      def singleBinderMultiProtoTreeMaker(patTrees: List[Tree], binderToSubst: Symbol, binderTp: Type, unsafe: Boolean = false): ProtoTreeMaker = {
        (patTrees, 
            { outerSubst: TreeXForm =>
                val binder = freshSym(currentOwner, patTrees.head.pos) setInfo binderTp
                val theSubst = mkTypedSubst(List(binderToSubst), List(CODE.REF(binder)), unsafe)
                println("theSubst: "+ theSubst)
                def nextSubst(tree: Tree): Tree = outerSubst(theSubst.transform(tree))
                (nestedTree => mkFun(binder, nextSubst(nestedTree)), nextSubst)
            })
      }


      /** `tree` is the cast (with failure expressed using the monad)
        * tp is the type for the binder of the next tree (`nestedTree`), it's assumed to already be used there, so no substitution is performed
        */
      def simpleProtoTreeMaker(tree: Tree, binder: Symbol): ProtoTreeMaker =
        (List(tree), 
          { outerSubst: TreeXForm =>
              (nestedTree => mkFun(binder,outerSubst(nestedTree)), outerSubst)
          })

      // shortcut: don't generate an extractor, TreeMaker only performs the substitution patBinder --> prevBinder
      def bindProtoTreeMaker(prevBinder: Symbol, patBinder: Symbol): ProtoTreeMaker =
        (List(), 
          { outerSubst: TreeXForm =>
              val theSubst = mkTypedSubst(List(patBinder), List(CODE.REF(prevBinder)), unsafe = true)
              def nextSubst(tree: Tree): Tree = outerSubst(theSubst.transform(tree))
              (nestedTree => nextSubst(nestedTree), nextSubst)
          })

      // def tryInstantiateType(tparams: List[Symbol], tp: Type, pt: Type): Type = if(tparams isEmpty) tp else {
      //   val tvars = tparams map freshVar
      //   val tpInst = tp.instantiateTypeParams(tparams, tvars)
      //   val targs = 
      //     if( tpInst <:< pt )
      //       try solvedTypes(tvars, tparams, tparams map (x => 0), false, lubDepth(List(tp, pt)))
      //       catch {
      //         case ex/*: NoInstance*/ => List()
      //       }
      //     else List()
        
      //   if(sameLength(tparams, targs)) tp.instantiateTypeParams(tparams, targs)
      //   else tp
      // }

      def binderOfExpectedType(prevBinder: Symbol, pt: Type, pos: Position)(implicit res: ListBuffer[ProtoTreeMaker]): Symbol =
          if(!(prevBinder.info <:< pt)) { 
            val castedBinder = freshSym(currentOwner, pos, "cp") setInfo pt 
            res += simpleProtoTreeMaker(mkCast(pt, prevBinder), castedBinder) // chain a cast before the actual extractor call
            castedBinder
          } else prevBinder

      def doUnapply(args: List[Tree], binderTypes: List[Type], extractor: Symbol, prevBinder: Symbol, pos: Position)(implicit res: ListBuffer[ProtoTreeMaker]): (List[Symbol], List[Tree]) = {
        val tp = prevBinder.info
        val extractorParamTp0 = extractor.tpe.paramTypes.head
        // val tparams = extractorParamTp0.typeArgs map (_.typeSymbol) // why are these different from extractorParamTp0.typeSymbol.typeParams?
        // val targs = extractorParamTp0.typeSymbol.typeParams map tp.memberType
        // val extractorParamTp = tryInstantiateType(tparams, extractorParamTp0, tp)
        // println("(extractorParamTp0, extractorParamTp0.typeArgs, extractorParamTp0.typeSymbol.typeParams)= "+ (extractorParamTp0, extractorParamTp0.typeArgs, extractorParamTp0.typeSymbol.typeParams))
        // println("(tp, tp.typeArgs, tp.typeSymbol.typeParams)= "+ (tp, tp.typeArgs, tp.typeSymbol.typeParams))
        // println("targs: "+ targs)
        // val baseTp = 
        val extractorParamTp = extractorParamTp0.instantiateTypeParams(extractorParamTp0.baseType(tp.typeSymbol).typeArgs map (_.typeSymbol), tp.typeArgs) // TODO: hack! use asSeenFrom? inference?

        // println("ASF "+ (extractorParamTp, tp.memberType(extractorParamTp0.baseType(tp.typeSymbol))))

        // example check: List[Int] <:< ::[B] (where B is ::'s type parameter) -- it's expected this fails, but that B in there should go away (ideally become Int)
        val prevBinderOrCasted = binderOfExpectedType(prevBinder, extractorParamTp, pos) // this may add to res

        // binderTypes != args map (_.tpe) since the args may have more specific types than the constructor's parameter types
        val sub@(patBinders, _) = 
            (args, binderTypes).zipped map { 
              case (BoundSym(b, p), _) => (b, p)
              case (p, tp) => (freshSym(currentOwner, pos, "p") setInfo tp, p)
            } unzip 

        res += patProtoTreeMaker(mkApply(extractor, prevBinderOrCasted), patBinders)

        sub
      }

      /** Decompose the pattern in `tree`, of shape C(p_1, ..., p_N), into a list of N symbols, and a list of its N sub-trees
        * The list of N symbols contains symbols for every bound name as well as the un-named sub-patterns (fresh symbols are generated here for these)
        *
        * @arg prevBinder  symbol used to refer to the result of the previous pattern's extractor (will later be replaced by the outer tree with the correct tree to refer to that patterns result)
        */
      def transformPat(prevBinder: Symbol, patTree: Tree)(implicit res: ListBuffer[ProtoTreeMaker]): (List[Symbol], List[Tree]) = patTree match {
        case UnApply(unfun, args) =>
          val binderTypes = analyzer.unapplyTypeList(unfun.symbol, unfun.tpe)

          doUnapply(args, binderTypes, unfun.symbol, prevBinder, patTree.pos)

        case Apply(fun, args)     =>
          val binderTypes = fun.tpe.paramTypes // not `args map (_.tpe)`, that's too strict
          val origSym = fun.asInstanceOf[TypeTree].original.symbol // undo rewrite performed in (5) of adapt
          val extractor = unapplyMember(origSym.filter(sym => reallyExists(unapplyMember(sym.tpe))).tpe)

          doUnapply(args, binderTypes, extractor, prevBinder, patTree.pos)


        case BoundSym(patBinder, p)          =>
          // binding a symbol is like the trivial pattern (corresponding to the extractor x => one(prevBinder)),
          // just keep it simple here, let optimisations fuse everything together later
          // res += singleBinderMultiProtoTreeMaker(List(mkSuccess(CODE.REF(prevBinder))), patBinder, prevBinder.info, unsafe = true)
          res += bindProtoTreeMaker(prevBinder, patBinder) // fused version

          (List(patBinder), List(p)) // the symbols are markers that may be used to refer to the result of the extractor in which the corresponding tree is nested -- it's the responsibility of the treemaker (added to res in the previous line) to replace this symbol by a reference that selects that result on the function symbol of the flatMap call that binds to the result of this extractor

        case Typed(expr, tpt)                 =>
          println("Typed: expr is wildcard, right? "+ expr)
          res += singleBinderProtoTreeMaker(mkCast(tpt.tpe, prevBinder), prevBinder)
          
          (Nil, Nil) // a typed pattern never has any subtrees

        case Literal(Constant(_)) | Ident(_) | Select(_, _) =>
          res += patProtoTreeMaker(mkCheck(mkEquals(prevBinder, patTree)), List())

          (Nil, Nil)
          
        case Alternative(alts)    => 
          val resAlts = new ListBuffer[ProtoTreeMaker]
          alts foreach (traverseDepthFirst(prevBinder, _)(resAlts))

          res += altsProtoTreeMaker(resAlts.toList, prevBinder)

          (Nil, Nil)

        // case x: ArrayValue        => // SequencePattern(x)
        // case x: Star              => 
        // case x: This              => 

        case _                       =>  // TODO
          println("UNHANDLED: "+ (prevBinder, patTree))
          (Nil, Nil)
      }


      def traverseDepthFirst(prevBinder: Symbol, patTree: Tree)(implicit res: ListBuffer[ProtoTreeMaker]): Unit = 
        if (!isWildcardPattern(patTree)) // skip wildcard trees -- no point in checking them
          transformPat(prevBinder, patTree).zipped foreach traverseDepthFirst

      val res = new ListBuffer[ProtoTreeMaker]
      traverseDepthFirst(scrutSym, pattern)(res)
      res.toList
    }

    def Xguard(guard: Tree): List[ProtoTreeMaker] = {
      if (guard == EmptyTree) List()
      else List(
        (List(mkCheck(X(guard))),
          { outerSubst => 
            val binder = freshSym(currentOwner, guard.pos) setInfo UnitClass.tpe
            (nestedTree => mkFun(binder, outerSubst(nestedTree)), outerSubst) // guard does not bind any variables, so next subst is the current one
          }))
    }

// tree exegesis, rephrasing everything in terms of extractors 

    /** A conservative approximation of which patterns do not discern anything.
      * A corrolary of this is that they do not entail any variable binding.
      */
    def isWildcardPattern(pat: Tree): Boolean = pat match {
      case Bind(_, body)        => isWildcardPattern(body)
      case Ident(nme.WILDCARD)  => true
      case x: Ident             => treeInfo.isVarPattern(x)
      case Alternative(ps)      => ps forall isWildcardPattern
      case EmptyTree            => true
      case _                    => false
    }
    
    object BoundSym {
      def unapply(t: Tree): Option[(Symbol, Tree)] = t match {
        case t@Bind(n, p) => 
          val b = t.symbol
          assert(b ne null, t); assert(b ne NoSymbol, t)
          Some((b, p))
        case _ => None
      }
    }


// code gen

    var ctr = 0
    def freshSym(owner: Symbol, pos: Position, prefix: String = "x") = {ctr += 1; assert(owner ne null); assert(owner ne NoSymbol); new TermSymbol(owner, pos, (prefix+ctr).toTermName)}

    // we must explicitly type the trees that we replace inside some other tree, since the latter may already have been typed, and will thus not be retyped
    // thus, we might end up with untyped subtrees inside bigger, typed trees
    def mkTypedSubst(from: List[Symbol], toMaybeUntyped: List[Tree], unsafe: Boolean = false) = {
      val toTyped = (from, toMaybeUntyped).zipped map { case (sym, tree) =>
        // if tree happened to be typed already, typed will bail out early -- that's ok
        if(unsafe) typed(tree) else typed(tree, EXPRmode, sym.info) // when swapping out a symbol for a new tree, that tree must conform to the symbol's type
      }
      new TreeSubstituter(from, toTyped)
    }

    def matcherTycon: Type = OptionClass.typeConstructor // TODO: determined by solving M in `implicitly[MatchingStrategy[M]]`
    // def tupleTypeOrUnit(tps: List[Type]): Type = if(tps.isEmpty) UnitClass.tpe else tupleType(tps)
    def matchingStrategyTycon = getMember(PredefModule, "MatchingStrategy".toTypeName)
    def Predef_implicitly = getMember(PredefModule, "implicitly".toTermName)

    import CODE._
    // TODO: lift out this call and cache the result?
    def mkImplicitMatcher: Tree = TypeApply(REF(Predef_implicitly), List(TypeTree(appliedType(matchingStrategyTycon.typeConstructor, List(matcherTycon)))))  // implicitly[MatchingStrategy[matcherTycon]].fail
    def mkFail: Tree = mkImplicitMatcher DOT "zero".toTermName 
    def mkSuccess(res: Tree): Tree = (mkImplicitMatcher DOT "one".toTermName)(res)
    def mkOrElse(thisCase: Tree, elseCase: Tree): Tree = (thisCase DOT "orElse".toTermName)(elseCase)
    def mkApply(fun: Tree, arg: Tree): Tree = fun APPLY arg
    def mkApply(fun: Symbol, arg: Symbol): Tree = REF(fun) APPLY REF(arg)
    def mkFlatMap(a: Tree, b: Tree): Tree = (a DOT "flatMap".toTermName)(b)
    def mkFun(arg: Symbol, body: Tree): Tree = Function(List(ValDef(arg)), body)
    def mkPatBinderTupleSel(binder: Symbol)(i: Int): Tree = (REF(binder) DOT ("_"+i).toTermName) // make tree that accesses the i'th component of the tuple referenced by binder
    
    def mkCheck(t: Tree, then: Tree = UNIT): Tree = (mkImplicitMatcher DOT "guard".toTermName)(t, then)
    def mkCast(expectedTp: Type, binder: Symbol): Tree = 
      mkCheck((REF(binder) setType binder.info) IS_OBJ expectedTp, (REF(binder) setType binder.info) AS expectedTp)
    def mkOr(as: List[Tree], f: Tree): Tree = (mkImplicitMatcher DOT "or".toTermName)((f :: as): _*)

    def mkEquals(binder: Symbol, other: Tree): Tree = REF(binder) MEMBER_== other
    // def mkApplyUnapply(fun: Tree, arg: Symbol): Tree = 
    //   (fun match {
    //     case UnApply(t, _) => REF(treeInfo.methPart(t).symbol)
    //     case t => t
    //   }) APPLY REF(arg)
  }
}