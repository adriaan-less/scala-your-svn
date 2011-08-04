object Test extends App {
 List(1,2,3) match { case x :: y :: xs => println(y) }
}