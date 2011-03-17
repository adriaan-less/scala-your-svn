//2
trait Base[A, This <: Base[A, This]] {
  trait Transformed[B] {
    override def toString: String = error("")
  }
  
  trait Reversed extends Transformed[A] {
    private def x(): Unit = {
      class a extends Transformed[A]
      // generating the sig for a's ctor causes the problem
      // adding generic sig constructor a$1:(arg$outer: Reversed$class.this.type)a$1
      // error: java.lang.Error: A in trait Base cannot be instantiated from [A,This <: Base[A,This]]Base[A,This]
      // ...
      // at scala.tools.nsc.symtab.Types$Type.asSeenFrom(Types.scala:544)
      // at scala.tools.nsc.symtab.Types$Type.memberInfo(Types.scala:558)
      // at scala.tools.nsc.backend.jvm.GenJVM$BytecodeGenerator$$anonfun$5.apply(GenJVM.scala:569)
    }
  }
}