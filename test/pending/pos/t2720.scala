object Test {
  class Q
  implicit def string2q(s: String): Q = new Q

  val x: Option[String] = Some("abc")
  
  val o: Any =  ((x.getOrElse("")): Q) 
}