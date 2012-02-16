class A[m[+x] <: Option[x]]
object ok extends A[Some]

class B[m[x] <: Option[x]]
object notOkVariance extends B[Some]
object notOkList extends A[List]
object notOkListB extends B[List]
