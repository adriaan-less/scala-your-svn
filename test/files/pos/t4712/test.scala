object Problem { 
  def a(i:IDerivedInterface) = i.f(classOf[String]) // error!
  def b(i:IDerivedInterface) = i.g(classOf[String]) 
}