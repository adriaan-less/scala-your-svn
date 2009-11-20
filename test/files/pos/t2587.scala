trait Copyable[T] { this : T with Copyable[T] =>
  def copy: T with Copyable[T] = this
}

class Field[T](val value: Object) extends Copyable[Field[T]] {
  override def copy: Field[T] = null
}

class Form() {
  def xxx (f: Field[_]) = f.copy
}