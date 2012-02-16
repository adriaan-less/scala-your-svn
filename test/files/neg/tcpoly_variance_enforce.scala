trait coll[m[+x]]

trait coll2[m[-x]]

trait coll3[m[x]]

trait coll4[m[x <: y], y]

class FooInvar[x]                 
class FooContra[-x]
class FooCov[+x]
class FooString[x <: String]

object fcollok extends coll[FooCov]
object fcollinv extends coll[FooInvar]      
object fcollcon extends coll[FooContra]     // error
object fcollwb extends coll[FooString]      // error
                                            
object fcoll2ok extends coll2[FooCov]       // error
object fcoll2inv extends coll2[FooInvar]    
object fcoll2con extends coll2[FooContra]   
object fcoll2wb extends coll2[FooString]      // error
                                            
object fcoll3cov extends  coll3[FooCov]     // error
object fcoll3inv extends coll3[FooInvar]    
object fcoll3con extends coll3[FooContra]   // error
object fcoll3wb extends  coll3[FooString]   // error

object fcoll4ok extends  coll4[FooString, String]
object fcoll4_1 extends  coll4[FooString, Int] // error
object fcoll4_2 extends  coll4[FooString, Any] // error


object test {
  var ok: coll[FooCov] = _
   
  def x: coll[FooInvar] = error("foo") 
  def y: coll[FooContra] = error("foo") // error
}


// TODO: need test for rank N with N >: 2
