abstract class Test {
 type t[m[x] <: Bound[x], Bound[x]]
 class Super[X]
 class Sub[X] extends Super[X]
 val x: t[Sub, Super]
}
