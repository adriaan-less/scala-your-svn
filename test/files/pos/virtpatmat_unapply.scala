object IntList {
  def unapply(il: IntList): Option[(Int, IntList)] = Some(il.hd, il.tl)
  def apply(x: Int, xs: IntList) = new IntList(x, xs)
}
class IntList(val hd: Int, val tl: IntList)

object Test {
  IntList(1, null) match { 
    case IntList(a1, IntList(a2, IntList(a3, y))) => a3
    case IntList(x, y) => x
  }
}

/*
  ((x1: IntList) => 
    IntList.this.unapply(x1).flatMap[Int](((x4: (Int, IntList)) => 
      IntList.this.unapply(x4._2).flatMap[Int](((x5: (Int, IntList)) => 
        IntList.this.unapply(x5._2).flatMap[Int](((x6: (Int, IntList)) => 
          Predef.this.implicitly[scala.Predef.MatchingStrategy[Option]](scala.this.Predef.OptionMatching).success[Int](x6._1))))))).orElse[Int](
    IntList.this.unapply(x1).flatMap[Int](((x7: (Int, IntList)) => 
      Predef.this.implicitly[scala.Predef.MatchingStrategy[Option]](scala.this.Predef.OptionMatching).success[Int](x7._1)))).orElse[Int](
    Predef.this.implicitly[scala.Predef.MatchingStrategy[Option]](scala.this.Predef.OptionMatching).fail)
  ).apply(IntList.apply(1, null))
*/