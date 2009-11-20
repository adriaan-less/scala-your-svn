object Test {
  class Foo
  class Bar
  implicit def foo2bar(f: Foo): Bar = new Bar
  implicit def triggerImplicitInBody(implicit f: Foo): Bar = f
}