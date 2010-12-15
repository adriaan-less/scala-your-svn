trait FooBar {
  val IR: FooOpsExp
  import IR._

  def foobar(rhs: Def[_]) : Unit = rhs match {
      case m: DeliteOpMap[_,_,creatingThisTypeVarFails] =>
  }
}
