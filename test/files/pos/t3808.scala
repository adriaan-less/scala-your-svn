object Test {
  def meh: Unit = {
    trait TC[I]
    implicit val tci = new TC[Int]{}

    def baz[J : TC] : String = "meh"

    /*val x =*/ baz // should infer baz[Int](tci) whether the val x = is there or not
  }
}