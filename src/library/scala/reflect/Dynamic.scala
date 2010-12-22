package scala.reflect

/** marker trait that reifies member selection on its instances

rewrite rule (in adaptToMember before we try implicits?)
typeOf(e) <: Dynamic (without coercions)
-----------------------------------------
       e.m -> e.dot("m")


use case: 
-  ship javascript objects,
  be able to call any method on them,
  no need to do anything to objects passed to javascript
  
- language virtualization
   arguments to method calls need to have static type Proxy[T] so that implicits can trigger the conversion to closures 
*/
trait Dynamic {
  // can we have a single definition here? should we?
  // ( cf for comprehensions, where the expansion is purely syntactic because we couldn't give a sufficiently generic signature, 
  //   we'd like to specify one to improve error messages now we can, but backwards compatibility prevents it)

  // so what would different signatures look like?

  // simple one-shot field selection
  def dot(name: String): Any

  // field selection whose result again admits dynamic behavior
  def dot(name: String): Dynamic

  // `name` denotes a method whose arguments should be proxies (so they can be lifted by implicits)
  // the Ti are not strictly necessary, but we must at least have Proxy as a top-level type constructor
  // also, note the need for arity polymorphism -- can we use a repeated Proxy[T forSome {type T}] ?
  def dot(name: String): (Proxy[T1], ..., Proxy[Tn]) => T

  // running with implicits (needs hypothetical literals-as-paths extension)
  trait SigFor[T] {
    type Sig
    def apply: Sig
  }
  
  implicit object onClickSig extends SigFor["onClick".type] { 
    type Sig = Proxy[Event => Unit] => Unit
    def apply(handler: Proxy[Event => Unit]): Unit = ...
  }
  // ...

  def dot[T](name: String)(implicit ev: SigFor[name.type]): ev.Sig = ev

}