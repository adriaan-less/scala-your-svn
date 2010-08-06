object Test{
  def ?[S <: AnyRef](implicit w : S) : w.type = w
  
  implicit def ZeroZipWith[S] = new ZipWith[S] {
    type T = Stream[S]
    def zipWith = error("")
  }    

  // bug?: return type should be inferred instead of "error: recursive method apply needs result type"
  def apply[S: ZipWith](s : S) = ?[ZipWith[S]].zipWith(s) 
  // def apply[S](s : S)(implicit zw: ZipWith[S]) = zw.zipWith(s) // this works
  
  trait ZipWith[S] {
    type T
    def zipWith : S => T
  }
}  