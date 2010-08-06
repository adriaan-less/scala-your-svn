object Test{
  trait ZW[S]{type T}
  def ZipWith[S, M <: ZW[S]]: M#T = error("ZW")

  // meh must be parameterised to force an asSeenFrom that 
  // duplicates the refinement in the TR's pre without updating its sym
  def meh[A] = ZipWith[A, ZW[A]{type T=Stream[A]}]

  meh[Int]: Stream[Int]
}