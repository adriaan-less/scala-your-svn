class Foo { def f = 1; def g[T] = 2; def f0() = 3; def g0[T]() = 4 }

// these should match (?)
class F0 extends Foo { override def f() = 1}
class F3 extends Foo { override def g[T]() = 1} // error??
class F4 extends Foo { override def f0 = 1}
class F7 extends Foo { override def g0[T] = 1} // error??

// these shouldn't
class F1 extends Foo { override def f[T]() = 1} // error
class F2 extends Foo { override def g() = 1} // error
class F5 extends Foo { override def f0[T] = 1} // error
class F6 extends Foo { override def g0 = 1} // error
