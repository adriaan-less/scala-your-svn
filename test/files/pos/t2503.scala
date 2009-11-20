import scala.collection.mutable._

trait SB[A] extends Buffer[A] {

  import collection.Traversable

  abstract override def insertAll(n: Int, iter: Traversable[A]): Unit = synchronized {
     super.insertAll(n, iter)
  }

  abstract override def update(n: Int, newelem: A): Unit = synchronized {
    super.update(n, newelem)
  }
}

class Foo extends ArrayBuffer[Int] with SB[Int]
