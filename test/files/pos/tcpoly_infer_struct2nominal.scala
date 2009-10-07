object Test extends Application {
trait MyIterator[T]

trait MyIterable[T] {  
  def iterator: MyIterator[T]
}
// generic way of instantiating the nominal type from its constituent members
implicit def makeMyIterable[T]: CanMake[MyIterable]#At[T]
  = mems => new MyIterable[T]{def iterator: MyIterator[T] = mems(0)()}
  
trait MyIterable2[T] {
  def iterator: MyIterator[T]
}
// generic way of instantiating the nominal type from its constituent members
implicit def makeMyIterable2[T]: CanMake[MyIterable2]#At[T]
  = mems => new MyIterable2[T]{def iterator: MyIterator[T] = mems(0)()}

trait CanMake[I[x]] { type At[T] = List[() => MyIterator[T]] => I[T]}
type HasMyIterator[T] = {def iterator: MyIterator[T]}
// generic way of instantiating the structural type by instantiating the corresponding nominal type from its members
// implicit def makeMyIterableX[T, S <: HasMyIterator[T]](iter: => MyIterator[T]): S = {
//     implicit val i = iter
//     iter
// }
 // def makeMyIterableX[T, S <: HasMyIterator[T]](iter: => MyIterator[T]): S = (iter, ())


implicit def hasIterator[T: CanMake[I]#At, I[x] <: HasMyIterator[x]](struct: HasMyIterator[T]): I[T]  = {  //(implicit c: ClassManifest[I[T]]) to force I[T] to be a class --> leads to divergence --> bug?
  val builder = implicitly[CanMake[I]#At[T]]
  builder(List(() => struct.iterator))
}
 
 
val x: MyIterable[Int] = new {def iterator: MyIterator[Int] = error("meh!")}

println(x.iterator)  
// implicit def hasIterator1[T](struct: HasMyIterator[T]): MyIterable[T]
//   = new MyIterable[T]{def iterator: MyIterator[T] = struct.iterator}
// 
// implicit def hasIterator2[T](struct: HasMyIterator[T]): MyIterable2[T]
//   = new MyIterable2[T]{def iterator: MyIterator[T] = struct.iterator}
}