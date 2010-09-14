object Test {
  class NumOps { def foo = 7 }
  class Rep[+T]
  implicit def numOps[T:Numeric](x:Rep[T]): NumOps = new NumOps

  class Var[+T]
  implicit def chainReadVar[T,U](v:Var[T])(implicit f: Rep[T]=>U): U = error("meh") //f(readVar(v))
  val v: Var[Double] = error("")
  v.foo // 7
}

  // def readVar[T](v:Var[T]): Rep[T] = new Rep[T]
  // readVar(v).foo // typechecks
  // chainReadVar(v).foo // typechecks
