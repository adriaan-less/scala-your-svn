trait FooOpsExp {

  abstract class Def[B]

  abstract class DeliteCollection[A]

  abstract case class DeliteOpMap[A,B,C[X] <: DeliteCollection[X]]() extends Def[C[B]]
}