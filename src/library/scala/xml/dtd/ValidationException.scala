/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml
package dtd


case class ValidationException(e: String) extends Exception(e)

/**
 *  @author Burak Emir
 */
object MakeValidationException {
  def fromFixedAttribute(k: String, value: String, actual: String) =
    ValidationException("value of attribute " + k + " FIXED to \""+
                        value+"\", but document tries \""+actual+"\"")

  def fromNonEmptyElement() =
    new ValidationException("element should be *empty*")

  def fromUndefinedElement(label: String) = 
    new ValidationException("element \""+ label +"\" not allowed here")

  def fromUndefinedAttribute(key: String) = 
    new ValidationException("attribute " + key +" not allowed here")

  def fromMissingAttribute(allKeys: scala.collection.Set[String]) = {
    val sb = new StringBuilder("missing value for REQUIRED attribute")
    if (allKeys.size > 1) sb.append('s');
    val it = allKeys.iterator
    while (it.hasNext) {
      sb.append('\'').append(it.next).append('\'') 
    }
    new ValidationException(sb.toString())
  }

  def fromMissingAttribute(key: String, tpe: String) =
    new ValidationException("missing value for REQUIRED attribute "+key+" of type "+tpe)

}
