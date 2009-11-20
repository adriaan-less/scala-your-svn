class Outer[T](val t: T) {
  class Inner {
    def getT : T = t
  }
}

class OuterImpl(x: X) extends Outer(x) {
  def newInner: Outer[X]#Inner = new Inner
}

class X {
  def getI : OuterImpl#Inner = {
    val oImpl = new OuterImpl(this)
    new oImpl.Inner
  }
}