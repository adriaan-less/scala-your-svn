trait A {
 type T <: S[Int] forSome { type S[_] }
 def f(x : T) : S[Int] forSome { type S[_] } = x
}