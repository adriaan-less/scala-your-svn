object O
class C { def m = 1}
object Test {
  implicit def o2c(o: O.type): C = new C
  // o2c(O) // this type checks, the implicit o2c is in scope, thus it should be inferred below
  O.m
}