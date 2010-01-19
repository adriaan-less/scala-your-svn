class X[Y[Z]]

object Test {
  def f(implicit m: scala.reflect.Manifest[X[List]]) {}
  f
}