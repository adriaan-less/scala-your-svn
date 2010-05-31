object t2416 {
  trait A { type B[X <: Double] = Int}

  type C[Z <: A] = Z#B[Boolean]
}