trait Updateable[UA]{
  def update(el: UA): Unit
}

trait BufferLike[BA] extends Updateable[BA]

trait IndexedSeqLike[IA] extends Updateable[IA]

trait ResizableArray[RA] extends IndexedSeqLike[RA] {
  def update(el: RA): Unit = error("")
}

class ArrayBuffer[AA] extends BufferLike[AA] with IndexedSeqLike[AA] with ResizableArray[AA] 
// the concrete update in ResizableArray matches the abstract one in BufferLike


trait SB[A] extends BufferLike[A] {
  abstract override def update(el: A): Unit = super.update(el) 
  // at this point update is resolved to update in BufferLike
}

class Foo extends ArrayBuffer[Int] with SB[Int]