trait TC[M, N]

object Test {
  implicit object StringIntTc extends TC[String, Int]

  implicit def enrichM[M, N](x: M)(implicit tc: TC[M, N]) = new {
   val foo = 0 
  }
  "".foo
  // error: could not find implicit value for parameter tc: this.TC[java.lang.String,N]
}