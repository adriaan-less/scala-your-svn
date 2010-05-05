class A
class B[T <: A](cons: T)

object C extends B(new A{})
