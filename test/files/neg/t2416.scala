object t2416a {
  trait A[X <: Double] { type B = X }
  def x : A[Int]#B = 10
}

object t2416b {
  trait A{type B[X <: Double] = Int}
  def x : A#B[Boolean] = 10
}

object t2416c {
  trait A{type B[X <: Double] = Int}
  type C[Z <: A] = Z#B[Boolean]
}