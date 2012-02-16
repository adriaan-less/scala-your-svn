// see #13
abstract class M {
  type T
  final type selfType = M {type T <: M.this.T}
  type actualSelfType >: M.this.type <: selfType

  def next: selfType

  // I don't understand why this doesn't compile, but that's a separate matter
  // error: method all2 cannot be accessed in M.this.selfType
  // because its instance type => Stream[M{type T <: M.this.selfType#T}]
  // contains a malformed type: M.this.selfType#T
  // def all2: Stream[M {type T <: self.T}] = Stream.cons(self: actualSelfType, next.all2)


  // compiles successfully
  // def all3: Stream[M {type T <: self.T}] = all3Impl(self: actualSelfType)
  // private def all3Impl(first: M {type T <: self.T}): Stream[M {type T <: self.T}] = Stream.cons(first, all3Impl(first.next))

  def all4: Stream[M {type T <: M.this.T}] = Unrelated.all4Impl(this: actualSelfType)
}

object Unrelated {
  // def all4Impl[U](first: M {type T <: U}): Stream[M {type T <: U}] = Stream.cons(first, all4Impl/*[U]*/(first.next))
  def all4Impl(first: M): Stream[M {type T <: first.T}] = Stream.cons(first: M{type T <: first.T}, all4Impl(first.next))
}
