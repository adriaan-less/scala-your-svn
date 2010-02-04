object Test extends Application {
  def precise(x: String)(y: x.type): x.type = {
    println(x+y)
    y
  }
  val foo = "foo"
  val fun : foo.type => foo.type = precise(foo)
  val bar : foo.type = precise(foo)(foo)
}