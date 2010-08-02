object Test{
  trait Meh[S]{type T}
  implicit def mehFun[A, B] = new Meh[A => B]{type T = Stream[A] => Stream[B]}
  def ZipWith[S](s: S)(implicit w: Meh[S]): w.T = error("meh")
  def map[A, B](f: A => B) = ZipWith(f) 
  val tst: Stream[Int] = map{x: String => x.length}/*.apply*/(Stream("a"))
  // the info of the implicit apply method---------^^^^^^^^^^ is wrong
}  
// other bug:
  // def ?[S <: AnyRef](implicit w : S) : w.type = w
  // val tst: Stream[Int] = ZipWith{x: String => x.length}(?)(Stream("a"))
