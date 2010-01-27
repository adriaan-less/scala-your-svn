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