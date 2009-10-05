trait FOO[B, m[A <: B]]
trait FOO2[A <: B, B]
trait FOO3[m[A <: B], B]

class Test {
  trait Bound[x]
  trait Bounded[x <: Bound[x]] extends Bound[x]
  def foo[a[x <: b[x]] <: b[x], b[_], c <: d, d] = "a"
  foo[Bounded, Bound, Any, Any]
}
//trait Idiom[idi[x]] { def foo: idi[Int]}
