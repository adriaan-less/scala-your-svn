trait ScalaServletModule extends ServletModule {
  def filter2(strs: String*) = {
    println(super.filter(strs:_*)) // "super." is essential
  }
}

object Test extends Application {
  (new ServletModule with ScalaServletModule).filter2("/rest")
}