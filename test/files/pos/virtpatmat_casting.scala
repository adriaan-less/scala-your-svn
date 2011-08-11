object Test extends App {
  List(1,2,3) match { case Nil => List(0) case x :: y :: z :: a :: xs => xs ++ List(x) }
}
