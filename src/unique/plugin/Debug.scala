package annotation.checker

import scala.tools.nsc.SubComponent

trait Debug { self: SubComponent =>
  import self.global._

  def printClass(s: String, x: AnyRef) =
    vprintln(s+" "+x+" ["+x.getClass+"]")

  def vinfoClass(s: String, x: AnyRef) =
    vinfo(s+" "+x+" ["+x.getClass+"]")

  var verbose = false
  var verbLvl = 3

  def vlprint(l: Int, x: => Any): Unit =
    if (verbose && l >= verbLvl) println("[unique] "+x)
  def vinfo(x: => Any): Unit =
    if (verbose && verbLvl >= 2) println("[unique] "+x)
  def vprintln(x: => Any): Unit =
    if (verbose && verbLvl >= 3) println("[unique] "+x)
  def verror(unit: CompilationUnit, tree: Tree, s: String) = {
    vinfo("ERROR: "+s)
    //unit.warning(tree.pos, s)
    unit.error(tree.pos, s)
  }
  def vwarn(unit: CompilationUnit, tree: Tree, s: String) = {
    vinfo("WARNING: "+s)
    //unit.warning(tree.pos, s)
    unit.warning(tree.pos, s)
  }
}
