object Test{
  trait ZW[S]{type T}
  def ZipWith[S, M <: ZW[S]]: M#T = error("ZW")
  def meh[A] = ZipWith[A, ZW[A]{type T=Stream[A]}]

  meh[Int]: Stream[Int]
}