object Test {
  class NumOps { def foo = 7 }
  class Rep[+T]
  implicit def numOps[T:Numeric](x:Rep[T]): NumOps = new NumOps
  class Var[+T]
  def readVar[T](v:Var[T]): Rep[T] = new Rep[T]
  implicit def chainReadVar[T,U](v:Var[T])(implicit f: Rep[T]=>U): U = f(readVar(v))
  val v = new Var[Double]
  readVar(v).foo // 7
  chainReadVar(v).foo // 7
  v.foo // 7
}