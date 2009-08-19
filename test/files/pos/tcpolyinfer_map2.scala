object Test {
  val x /*: List[Int] is inferred */ = (List(1,2,3), List(3,2,1)) map2 ((_: Int) + (_: Int))
}