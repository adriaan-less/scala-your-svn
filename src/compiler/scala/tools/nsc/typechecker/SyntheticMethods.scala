/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package typechecker

import symtab.Flags
import symtab.Flags._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Synthetic method implementations for case classes and case objects.
 *
 *  Added to all case classes/objects:
 *    def productArity: Int
 *    def productElement(n: Int): Any
 *    def productPrefix: String
 *    def productIterator: Iterator[Any]
 *
 *  Selectively added to case classes/objects, unless a non-default
 *  implementation already exists:
 *    def equals(other: Any): Boolean
 *    def hashCode(): Int
 *    def canEqual(other: Any): Boolean
 *    def toString(): String
 *
 *  Special handling:
 *    protected def readResolve(): AnyRef
 */
trait SyntheticMethods extends ast.TreeDSL {
  self: Analyzer =>

  import global._
  import definitions._
  import CODE._

  /** Add the synthetic methods to case classes.
   */
  def addSyntheticMethods(templ: Template, clazz0: Symbol, context: Context): Template = {
    if (phase.erasedTypes)
      return templ

    val synthesizer = new ClassMethodSynthesis(
      clazz0,
      newTyper( if (reporter.hasErrors) context makeSilent false else context )
    )
    import synthesizer._

    val originalAccessors = clazz.caseFieldAccessors
    // private ones will have been renamed -- make sure they are entered
    // in the original order.
    def accessors = clazz.caseFieldAccessors sortBy { acc =>
      originalAccessors indexWhere { orig =>
        (acc.name == orig.name) || (acc.name startsWith (orig.name append "$"))
      }
    }
    val arity = accessors.size
    // If this is ProductN[T1, T2, ...], accessorLub is the lub of T1, T2, ..., .
    // !!! Hidden behind -Xexperimental due to bummer type inference bugs.
    // Refining from Iterator[Any] leads to types like
    //
    //    Option[Int] { def productIterator: Iterator[String] }
    //
    // appearing legitimately, but this breaks invariant places
    // like Manifests and Arrays which are not robust and infer things
    // which they shouldn't.
    val accessorLub  = (
      if (opt.experimental) {
        global.weakLub(accessors map (_.tpe.finalResultType))._1 match {
          case RefinedType(parents, decls) if !decls.isEmpty => intersectionType(parents)
          case tp                                            => tp
        }
      }
      else AnyClass.tpe
    )

    def forwardToRuntime(method: Symbol): Tree =
      forwardMethod(method, getMember(ScalaRunTimeModule, method.name prepend "_"))(This(clazz) :: _)

    // Any member, including private
    def hasConcreteImpl(name: Name) =
      clazz.info.member(name).alternatives exists (m => !m.isDeferred && !m.isSynthetic)

    def hasOverridingImplementation(meth: Symbol) = {
      val sym = clazz.info nonPrivateMember meth.name
      sym.alternatives filterNot (_ eq meth) exists { m0 =>
        !m0.isDeferred && !m0.isSynthetic && (typeInClazz(m0) matches typeInClazz(meth))
      }
    }
    def readConstantValue[T](name: String, default: T = null.asInstanceOf[T]): T = {
      clazzMember(newTermName(name)).info match {
        case NullaryMethodType(ConstantType(Constant(value))) => value.asInstanceOf[T]
        case _                                                => default
      }
    }
    def productIteratorMethod = {
      createMethod(nme.productIterator, iteratorOfType(accessorLub))(_ =>
        gen.mkMethodCall(ScalaRunTimeModule, nme.typedProductIterator, List(accessorLub), List(This(clazz)))
      )
    }
    def projectionMethod(accessor: Symbol, num: Int) = {
      createMethod(nme.productAccessorName(num), accessor.tpe.resultType)(_ => REF(accessor))
    }

    /** Common code for productElement and (currently disabled) productElementName
     */
    def perElementMethod(name: Name, returnType: Type)(caseFn: Symbol => Tree): Tree =
      createSwitchMethod(name, accessors.indices, returnType)(idx => caseFn(accessors(idx)))

    // def productElementNameMethod = perElementMethod(nme.productElementName, StringClass.tpe)(x => LIT(x.name.toString))

    /** The canEqual method for case classes.
     *    def canEqual(that: Any) = that.isInstanceOf[This]
     */
    def canEqualMethod: Tree = (
      createMethod(nme.canEqual_, List(AnyClass.tpe), BooleanClass.tpe)(m => 
        Ident(m.firstParam) IS_OBJ classExistentialType(clazz))
    )

    /** The equality method for case classes.
     *  0 args:
     *    def equals(that: Any) = that.isInstanceOf[this.C] && that.asInstanceOf[this.C].canEqual(this)
     *  1+ args:
     *    def equals(that: Any) = (this eq that.asInstanceOf[AnyRef]) || {
     *      (that.isInstanceOf[this.C]) && {
     *        val x$1 = that.asInstanceOf[this.C]
     *        (this.arg_1 == x$1.arg_1) && (this.arg_2 == x$1.arg_2) && ... && (x$1 canEqual this)
     *       }
     *    }
     */
    def equalsModuleMethod: Tree = localTyper typed {
      val method = equalsSym
      val that = method ARG 0
      
      localTyper typed {
        DEF(method) === { 
          (This(clazz) DOT Object_eq)(that AS AnyRefClass.tpe)
        }
      }
    }
    
    /** The canEqual method for case classes.  Note that if we spot
     *  a user-supplied equals implementation, we simply return true
     *  so as not to interfere.
     */
    def canEqualMethod: Tree = {
      val method  = syntheticMethod(nme.canEqual_, 0, makeTypeConstructor(List(AnyClass.tpe), BooleanClass.tpe))
      val that    = method ARG 0
      
      typer typed (DEF(method) === (that IS_OBJ clazz.tpe))
    }

    /** The equality method for case classes.  The argument is an Any,
     *  but because of boxing it will always be an Object, so a check
     *  is neither necessary nor useful before the cast.
     *
     *   def equals(that: Any) = 
     *     (this eq that.asInstanceOf[AnyRef]) || 
     *     (that match {
     *       case x @ this.C(this.arg_1, ..., this.arg_n) => x canEqual this  
     *       case _                                       => false
     *     })
     */
    def equalsClassMethod: Tree = {
      val method = equalsSym
      val that = method ARG 0
      val constrParamTypes = clazz.primaryConstructor.tpe.paramTypes
      
      // returns (Apply, Bind)
      def makeTrees(acc: Symbol, cpt: Type): (Tree, Bind) = {
        val varName     = context.unit.fresh.newName(clazz.pos.focus, acc.name + "$")
        val isRepeated  = isRepeatedParamType(cpt)
        val binding     = if (isRepeated) Star(WILD()) else WILD()
        val eqMethod: Tree  =
          if (isRepeated) gen.mkRuntimeCall(nme.sameElements, List(Ident(varName), Ident(acc)))
          else (varName DOT nme.EQ)(Ident(acc))

        (eqMethod, varName BIND binding)
      }
      
      // Creates list of parameters and a guard for each
      val (guards, params) = (clazz.caseFieldAccessors, constrParamTypes).zipped map makeTrees unzip

      // Verify with canEqual method before returning true.
      def canEqualCheck() = {
        val that: Tree              = typer typed ((method ARG 0) AS clazz.tpe)
        val canEqualOther: Symbol   = clazz.info nonPrivateMember nme.canEqual_
        
        (that DOT canEqualOther)(This(clazz))
      }
        
      // Pattern is classname applied to parameters, and guards are all logical and-ed
      val (guard, pat) = (AND(guards: _*), clazz.name.toTermName APPLY params)
      
      localTyper typed {
        DEF(method) === {
          (This(clazz) ANY_EQ that) OR (that MATCH(
            (CASE(pat) IF guard)  ==> canEqualCheck()        ,
            DEFAULT               ==> FALSE
          ))
        }
      }
    }

    def hasSerializableAnnotation(clazz: Symbol) =
      clazz hasAnnotation SerializableAttr

    def readResolveMethod: Tree = {
      // !!! the synthetic method "readResolve" should be private, but then it is renamed !!!
      val method = newSyntheticMethod(nme.readResolve, PROTECTED, makeNoArgConstructor(ObjectClass.tpe))
      typer typed (DEF(method) === REF(clazz.sourceModule))
    }

    def newAccessorMethod(tree: Tree): Tree = tree match {
      case DefDef(_, _, _, _, _, rhs) =>
        var newAcc = tree.symbol.cloneSymbol
        newAcc.name = context.unit.fresh.newName(tree.symbol.pos.focus, tree.symbol.name + "$")
        newAcc setFlag SYNTHETIC resetFlag (ACCESSOR | PARAMACCESSOR | PRIVATE)
        newAcc.privateWithin = NoSymbol
        newAcc = newAcc.owner.info.decls enter newAcc
        val result = typer typed { DEF(newAcc) === rhs.duplicate }
        log("new accessor method " + result)
        result
    }

    val ts = new ListBuffer[Tree]

    if (!phase.erasedTypes) try {
      if (clazz hasFlag Flags.CASE) {
        val isTop = !(clazz.ancestors exists (_ hasFlag Flags.CASE))
        // case classes are implicitly declared serializable
        clazz addAnnotation AnnotationInfo(SerializableAttr.tpe, Nil, Nil)

        if (isTop) {
          // If this case class has fields with less than public visibility, their getter at this
          // point also has those permissions.  In that case we create a new, public accessor method
          // with a new name and remove the CASEACCESSOR flag from the existing getter.  This complicates
          // the retrieval of the case field accessors (see def caseFieldAccessors in Symbols.)
          def needsService(s: Symbol) = s.isMethod && (s hasFlag CASEACCESSOR) && !s.isPublic
          for (stat <- templ.body ; if stat.isDef && needsService(stat.symbol)) {
            ts += newAccessorMethod(stat)
            stat.symbol resetFlag CASEACCESSOR
          }
        }
        
        // methods for case classes only
        def classMethods = List(
          Object_hashCode -> (() => forwardingMethod(nme.hashCode_, "_" + hashCodeTarget)),
          Object_toString -> (() => forwardingMethod(nme.toString_, "_" + nme.toString_)),
          Object_equals   -> (() => equalsClassMethod)
        )
        // methods for case objects only
        def objectMethods = List(
          Object_toString -> (() => moduleToStringMethod)
        )
        // methods for both classes and objects
        def everywhereMethods = {
          val accessors = clazz.caseFieldAccessors
          List(
            Product_productPrefix   -> (() => productPrefixMethod),
            Product_productArity    -> (() => productArityMethod(accessors.length)),
            Product_productElement  -> (() => productElementMethod(accessors)),
            // This is disabled pending a reimplementation which doesn't add any
            // weight to case classes (i.e. inspects the bytecode.)
            // Product_productElementName  -> (() => productElementNameMethod(accessors)),
            Product_canEqual        -> (() => canEqualMethod)
          )
        }
      }
      if (accessors.isEmpty)
        thatTest AND ((thatCast DOT nme.canEqual_)(This(clazz)))
      else
        argsBody
    }

    /** The _1, _2, etc. methods to implement ProductN.
     */
    def productNMethods = {
      val accs = accessors.toIndexedSeq
      1 to arity map (num => productProj(arity, num) -> (() => projectionMethod(accs(num - 1), num)))
    }

    // methods for both classes and objects
    def productMethods = {
      List(
        Product_productPrefix   -> (() => constantNullary(nme.productPrefix, clazz.name.decode)),
        Product_productArity    -> (() => constantNullary(nme.productArity, arity)),
        Product_productElement  -> (() => perElementMethod(nme.productElement, accessorLub)(Ident)),
        Product_iterator        -> (() => productIteratorMethod),
        Product_canEqual        -> (() => canEqualMethod)
        // This is disabled pending a reimplementation which doesn't add any
        // weight to case classes (i.e. inspects the bytecode.)
        // Product_productElementName  -> (() => productElementNameMethod(accessors)),
      )
    }

    def caseClassMethods = productMethods ++ productNMethods ++ Seq(
      Object_hashCode -> (() => forwardToRuntime(Object_hashCode)),
      Object_toString -> (() => forwardToRuntime(Object_toString)),
      Object_equals   -> (() => equalsClassMethod)
    )

    def caseObjectMethods = productMethods ++ Seq(
      Object_hashCode -> (() => constantMethod(nme.hashCode_, clazz.name.decode.hashCode)),
      Object_toString -> (() => constantMethod(nme.toString_, clazz.name.decode))
      // Not needed, as reference equality is the default.
      // Object_equals   -> (() => createMethod(Object_equals)(m => This(clazz) ANY_EQ Ident(m.firstParam)))
    )

    /** If you serialize a singleton and then deserialize it twice,
     *  you will have two instances of your singleton unless you implement
     *  readResolve.  Here it is implemented for all objects which have
     *  no implementation and which are marked serializable (which is true
     *  for all case objects.)
     */
    def needsReadResolve = (
         clazz.isModuleClass
      && clazz.isSerializable
      && !hasConcreteImpl(nme.readResolve)
    )

    def synthesize(): List[Tree] = {
      val methods = (
        if (!clazz.isCase) Nil
        else if (clazz.isModuleClass) caseObjectMethods
        else caseClassMethods
      )
      def impls = for ((m, impl) <- methods ; if !hasOverridingImplementation(m)) yield impl()
      def extras = (
        if (needsReadResolve) {
          // Aha, I finally decoded the original comment.
          // This method should be generated as private, but apparently if it is, then
          // it is name mangled afterward.  (Wonder why that is.) So it's only protected.
          // For sure special methods like "readResolve" should not be mangled.
          List(createMethod(nme.readResolve, Nil, ObjectClass.tpe)(m => { m setFlag PRIVATE ; REF(clazz.sourceModule) }))
        }
        else Nil
      )

      try impls ++ extras
      catch { case _: TypeError if reporter.hasErrors => Nil }
    }

    /** If this case class has any less than public accessors,
     *  adds new accessors at the correct locations to preserve ordering.
     *  Note that this must be done before the other method synthesis
     *  because synthesized methods need refer to the new symbols.
     *  Care must also be taken to preserve the case accessor order.
     */
    def caseTemplateBody(): List[Tree] = {
      val lb = ListBuffer[Tree]()
      def isRewrite(sym: Symbol) = sym.isCaseAccessorMethod && !sym.isPublic

      for (ddef @ DefDef(_, _, _, _, _, _) <- templ.body ; if isRewrite(ddef.symbol)) {
        val original = ddef.symbol
        val newAcc = deriveMethod(ddef.symbol, name => context.unit.freshTermName(name + "$")) { newAcc =>
          newAcc.makePublic
          newAcc resetFlag (ACCESSOR | PARAMACCESSOR)
          ddef.rhs.duplicate
        }
        ddef.symbol resetFlag CASEACCESSOR
        lb += logResult("case accessor new")(newAcc)
      }

      lb ++= templ.body ++= synthesize() toList
    }

    if (phase.id > currentRun.typerPhase.id) templ
    else treeCopy.Template(templ, templ.parents, templ.self,
      if (clazz.isCase) caseTemplateBody()
      else synthesize() match {
        case Nil  => templ.body // avoiding unnecessary copy
        case ms   => templ.body ++ ms
      }
    )
  }
}
