package annotation.checker

import scala.tools.nsc._
import scala.tools.nsc.transform._
import scala.tools.nsc.symtab._
import scala.tools.nsc.plugins._

import scala.tools.nsc.ast._

abstract class SwapTransform extends PluginComponent with Transform with Debug {
  // inherits abstract value `global' and class `Phase' from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.{typed, atOwner}    // methods to type trees

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new SwapTransformer(unit)

  class SwapTransformer(unit: CompilationUnit) extends Transformer {

    val uniqueMod = getModule("scala.annotation.UniqueOps")
    val swapFun = getMember(uniqueMod, "swap")

    override def transform(tree: Tree): Tree = tree match {
      case Apply(TypeApply(se, tptr), params) if (se.symbol == swapFun) =>
        params(0) match {
          case sel @ Select(qual, field) if (sel.symbol.isGetter) =>
            // OK, 1st param is field select

            val valueTpe = params(0).tpe.withoutAnnotations // TODO: remove only ours!

            val sym = currentOwner.newValue(tree.pos, unit.fresh.newName(tree.pos, "tmp"))
                        .setFlag(Flags.SYNTHETIC)
                        .setInfo(valueTpe)
            val firstExpr = typed(atPos(tree.pos)(ValDef(sym, params(0))))

            // obtain setter from getter
            val setter = sel.symbol.setter(sel.symbol.owner)
            val assign = typed(atPos(tree.pos)(
              Apply(Select(qual, setter), List(params(1)))))

            val result = typed(atPos(tree.pos)(Ident(sym)))

            treeCopy.Block(tree, List(firstExpr, assign), result)

          case _ =>
            verror(unit, tree, "first parameter of swap must be a field selection")
            tree
        }

      case _ => super.transform(tree)
    }
  }

}
