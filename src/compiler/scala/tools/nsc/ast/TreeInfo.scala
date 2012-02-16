/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

import reflect.internal.Flags._
import symtab._

/** This class ...
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class TreeInfo extends reflect.internal.TreeInfo {
  val global: Global
  import global._

  import definitions.ThrowableClass

  /** Is tree legal as a member definition of an interface?
   */
  def isInterfaceMember(tree: Tree): Boolean = tree match {
    case EmptyTree                    => true
    case Import(_, _)                 => true
    case TypeDef(_, _, _, _)          => true
    case DefDef(mods, _, _, _, _, _)  => mods.hasFlag(DEFERRED)
    case ValDef(mods, _, _, _)        => mods.hasFlag(DEFERRED)
    case DocDef(_, definition)        => isInterfaceMember(definition)
    case _ => false
  }

  /** Is tree a pure (i.e. non-side-effecting) definition?
   */
  override def isPureDef(tree: Tree): Boolean = tree match {
    case DocDef(_, definition) => isPureDef(definition)
    case _ => super.isPureDef(tree)
  }

 /** Does list of trees start with a definition of
   *  a class of module with given name (ignoring imports)
   */
  override def firstDefinesClassOrObject(trees: List[Tree], name: Name): Boolean = trees match {
    case ClassDef(_, `name`, _, _) :: Nil => true
    case _ => super.firstDefinesClassOrObject(trees, name)
  }
}
