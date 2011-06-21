object Problem { 
  def a(i:Derived) = i.f(classOf[String]) // error!
  def b(i:Derived) = i.g(classOf[String]) 
  def c(i:Derived) = i.g(i.bla) // error!
}