// this test probably should not compile, but I can't find where the spec specifies the expected behaviour
package mypkg {

  trait M { type C <: M }
  trait F extends M { type C = F }

  // requires private[mypkg] to reproduce
  private[mypkg] abstract class Private[E <: M] {
    type Sub <: Private[E#C]
    final val sub: Sub = null.asInstanceOf[Sub]

    val foo: E
  }

  // TODO: I'd expect a compiler error here: Private (qualified private) is less accessible than Public (public), and thus escapes its defining scope, right?
  // Private's private[mypkg] access boundary is still less accessible than public, right?
  trait Public[E <: M] extends Private[E] {
    type Sub = Public[E#C]
  }
}

import mypkg._

object Test {
  val seq = null.asInstanceOf[Public[F]]
  seq.sub.foo
}