trait Element[T]

trait Transform { 
  type T <: Element[T]

  def processBlock = new Array[T](1)
}
