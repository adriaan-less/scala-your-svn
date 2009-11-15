object Test {
  def meh(xs: List[Any]) {
    class X
    xs map { x =>  (new X) }
  }
}