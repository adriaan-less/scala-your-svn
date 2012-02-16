trait B
class D extends B
class C[T] {
	def foo(bar: T) = {
		bar match {
			case _: Array[T] => new D
			case _: Array[Array[T]] => new D
			case _ => new D
		}
	}
}
