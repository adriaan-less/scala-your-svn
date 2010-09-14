object Test {
  trait A[Ta]; trait B[Tb]
  implicit def a2b[T](a: A[T]): B[T] = error("magic")

  sealed abstract class ViewableAs[From, To] extends (From => To)
  object ViewableAs {
    implicit def conformsOrViewsAs[A, B](implicit c: A => B): A ViewableAs B = new (A ViewableAs B) {def apply(x: A) = x}
  }

  def convIndir[X, U](x: X)(implicit w: X ViewableAs B[U]): B[U] = x

  val a: A[Int] = error("who cares")
  
  // in order to type check the following expression, implicit resolution has to do the following:
  // do type inference on convIndir:
  //  X :> A[Int]
  // find implicit A[Int] ViewableAs B[T]
  // try conformsOrViewsAs
  //  compare A ViewableAs B to typeVarPt (A[Int] ViewableAs B[?T])
  // do type inference: 
  //  
  // find implicit A[Int] => B[?T]
  convIndir(a)
}

/*
Beginning implicit search for Test.this.convIndir[Test.A[Int], U](Test.this.a) expecting Test.ViewableAs[Test.A[Int],Test.B[U]]
  typing implicit with undetermined type params: List(type U)
  typed implicit Test.this.ViewableAs.conformsOrViewsAs:[A,B](implicit c: (A) => B)Test.ViewableAs[A,B], pt = Test.ViewableAs[Test.A[Int],Test.B[?U]]
    regBound: (?A,Test.this.A[scala.this.Int[]],true)
    regBound: (?A,Test.this.A[scala.this.Int[]],false)
    regBound: (?B,Test.this.B[?U],true)
    regBound: (?B,Test.this.B[?U],false)
    setInst: (?A,Test.this.A[scala.this.Int[]])
    setInst: (?B,Test.this.B[?U])
    Beginning implicit search for Test.this.ViewableAs.conformsOrViewsAs[Test.A[Int], Test.B[?U]] expecting (Test.A[Int]) => Test.B[?U]
      typed implicit Test.this.a2b:[T](a: Test.A[T])Test.B[T], pt = (Test.A[Int]) => Test.B[?U]
      regBound: (?T,scala.this.Int[],false)
      regBound: (?T,scala.this.Int[],true)
      regBound: (?T,?U,true)
      regBound: (?U,?T,true)
      regBound: (?U,scala.this.Int[],true)
      setInst: (?T,?U)
      regBound: (?U,scala.this.Int[],true)
      regBound: (?U,scala.this.Int[],false)

      typed implicit scala.this.Predef.conforms:[A]<:<[A,A], pt = (Test.A[Int]) => Test.B[?U]
      regBound: (?A,Test.this.A[scala.this.Int[]],true)
      regBound: (?A,Test.this.B[?U],false)
      setInst: (?A,Test.this.A[scala.this.Int[]])

*/