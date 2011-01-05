class Dep(meh: Int)(implicit val depWit: String)

object Test extends Application {
  implicit val depWit: String = "meh"

  def Inner(implicit w: String) = 1

  class meh extends Dep(Inner){
    // when typing the default constructor, meh.this.depWit (inherited from Dep)
    // shadows Test.depWith, thus, when inferring the argument `w` in the call to Inner,
    // Test.depWith is not eligible statically (but meh.this.depWith is not yet initialised dynamically)
    // shouldn't inSelfSuperCall in Context keep  meh.this.depWit out of the implicitss?
  /*
    class meh extends Dep {
      def this() {
        this(Test.this.Inner(meh.this.depWit))(Test.this.depWit)
      }
    }
  */
  }
  new meh
}
