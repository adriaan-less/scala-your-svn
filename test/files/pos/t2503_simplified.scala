trait BufferLike[BA] {
  def update(el: BA): Unit
}

trait ResizableArray[RA] {
  def update(el: RA): Unit = error("")
}

class ArrayBuffer[AA] extends BufferLike[AA] with ResizableArray[AA] 
// the concrete update in ResizableArray matches the abstract one in BufferLike


trait SB[A] extends BufferLike[A] {
  abstract override def update(el: A): Unit = super.update(el) 
  // at this point update is resolved to update in BufferLike
  // I would expect `abstract override` to mean overriding a member that matches `update` in `BufferLike`
}

class Foo extends ArrayBuffer[Int] with SB[Int]
// should the super call in SB really resolve to update in ResizableArray?
// IMO, this is wrong: it's an accidental override, because, technically, 
// ResizableArray does not know about BufferLike

// it works fine when there's a common supertype for BufferLike and ResizableArray that declares update
// (say, Updateable, similar to Addable,...)
// thus, I think this ticket is a dupe of #2497
