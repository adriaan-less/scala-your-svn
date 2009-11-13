object Test {
  def x : T[Int] forSome { type T[_] } = x
}