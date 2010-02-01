trait Selector
trait Closeable
trait Disposable
trait DatagramSocket

class X {
  def disposes[T <: Selector](abstractSelector: T) { }
  def disposes[T <: Closeable](closeable: T) { }
  def disposes[T <: Disposable](disposable: T) { }
  def disposes(datagramSocket: DatagramSocket): DatagramSocket = datagramSocket
}

// for easy testing, this must also still work:
trait Monad[T <: Bound[T], MyType[x <: Bound[x]], Bound[_]] {
  def map[S <: Bound[S]](f: T => S): MyType[S]  // etc
}                                                                               
class Set[T <: Ordered[T]] extends Monad[T, Set, Ordered] {
  def map[S <: Ordered[S]](f: T => S): Set[S] = error("TODO") // etc
}                                                                   