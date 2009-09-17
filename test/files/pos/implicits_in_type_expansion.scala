object Test {
  sealed abstract class =:=[From, To] extends (From => To)
  implicit def typeEquals[A]: A =:= A = new (A =:= A) {def apply(x: A) = x}

  sealed abstract class Test[Tst] {
    type If[True, False]
  }
  
  case class FalseTest[Tst] extends Test[Tst] {
    type If[True, False] = False
  }
  
  implicit def TrueTest[Tst](implicit witness: Tst) = new TrueTest[Tst]
  class TrueTest[Tst](implicit val witness: Tst) extends Test[Tst] {
    type If[True, False] = True // TODO: test case where this type member is commented out
    // in that case, expect compile error, as singleton type gets widened to type on which a type is selected that is not an alias
    // (tested manually and seems ok)
  }
  
  // TODO: test case to check this type param C gets instantiated correctly in the default value
  class If[C](implicit val witness: Test[C] = FalseTest[C]) { 
    type ThenElse[T, E] = witness.If[T, E]
  }

  val x : If[Int =:= String]#ThenElse[Boolean, String] = "Test"
  val y : If[Int =:= Int]#ThenElse[Boolean, String] = false

  // If[Int =:= Int]#ThenElse[Boolean, String] normalizes to If[Int =:= Int]#witness.type#If[Boolean,String]
  // now, it would be nice if we could expand (not normalize -- that's not sound) If[Int =:= Int]#witness.type to TrueTest[Int =:= Int]
  // since TrueTest[Int =:= Int]#If[Boolean,String] normalizes to Boolean 
  // 
}

/* when scalac is compiled with -Xelide-level FINE, output of compiling this test should be:
<adaptType> adapted implicit singleton type: Test.FalseTest[Test.=:=[Int,String]]
<adaptType> adapted implicit singleton type: Test.FalseTest[Test.=:=[Int,String]]
<adaptType> adapted implicit singleton type: Test.FalseTest[Test.=:=[Int,String]]
<adaptType> adapted implicit singleton type: Test.TrueTest[Test.=:=[Int,Int]]
<adaptType> adapted implicit singleton type: Test.TrueTest[Test.=:=[Int,Int]]
<adaptType> adapted implicit singleton type: Test.TrueTest[Test.=:=[Int,Int]]
*/