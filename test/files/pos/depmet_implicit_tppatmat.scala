// trait Foo
// trait Foos[F<:Foo] extends Seq[F] { type Elt = F }
// type Atom[F] = if (F<:Foos[_]]) F#Elt else F
// ...
// def frob(x:Atom[F]) = ...

// trait HO[-From, +To[_]] {
//   def apply[U](x: From): To[U]
// }


// needs compiler -Xexperimental compiler switch (see comment below)
trait CollectsCases[C, El, +U, +V] {
  def caseDef(xs: C): U
  def caseSeq(xs: Seq[El]): V
}

trait Id {
  type Apply[U] = U
}

trait Collects[C, El] {
  type CaseDef[T]
  type CaseSel[T]
  type DoMatch[CD, CS]
  type PMat[U] = CollectsCases[C, El, CaseDef[U], CaseSel[U]]
  def doMatch[U](x: C)(pmat: PMat[U]): U
}

class CollectsDefault {
  implicit def collectsDef[T] = new Collects[T, T] {
    type CaseDef[T] = T
    type CaseSel[T] = Any
    type DoMatch[CD, CS] = CD
    def doMatch[U](x: T)(pmat: PMat[U]) = pmat.caseDef(x)
  }
}

object Collects extends CollectsDefault {
  implicit def collectsSeq[C <: Seq[_], T](implicit ev: C <:< Seq[T]) = new Collects[C, T] {
    type CaseDef[T] = Any
    type CaseSel[T] = T
    type DoMatch[CD, CS] = CS
    def doMatch[U](x: C)(pmat: PMat[U]) = pmat.caseSeq(x)
  }
}

object Test extends Application {
  def frob[C, T](xs: C)(implicit ev: Collects[C, T]): ev.DoMatch[C, T] = 
    ev.doMatch(xs){new CollectsCases[C, T, C, T]{ // TODO: next step, give better type to partial function composition instead of ad-hoc visitor class CollectsCases
      def caseSeq(xs: Seq[T]) = xs.head
      def caseDef(xs: C) = xs
    }}
  
  println(frob(List(1, 2)))
  println(frob("not a List(1, 2)"))  
}