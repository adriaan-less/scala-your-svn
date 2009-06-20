trait A[B[D[X, Y <: X]]] {
 def f() : B[P]
}

trait C[E[T, S]]

trait P[U, V <: U]

trait Error extends A[C]

// C: (E: (* --> (*    --> *)) --> *(C[E],C[E]))
// B: ((X: * --> (*(X) --> *)) --> *)

//  (E: (* --> (*    --> *)) --> *(C[E],C[E])) <: ((X: * --> (*(X) --> *)) --> *)
  //  (* --> (*    --> *)) :> ((X: * --> (*(X) --> *))
    // (*) <: (X: *)
    // (*    --> *) :> (*(X) --> *)
      // * <: *(X)
        // [Nothing, Any] subinterval [Nothing, X]
        // Nothing <: Nothing  
        // Any <: X // ERROR!
      // * <: *
  //  *(C[E],C[E]) <: *
  

trait ExpectInv[X[_]]
trait Cov[+X]
  
trait Error2 extends ExpectInv[Cov]