// we have no control over the *.java files -- need to cast to generic supertype of MyHashMap (i.e., JSONObject) for interoperability
object Test {
  val hm = new MyHashMap().asInstanceOf[HashMap[String, Int]] // cast is needed to avoid error in comment below
  hm.put("a", 1)
/* error: overloaded method value put with alternatives:
  (K,V)V <and>
  ((x$1: IK with K,x$2: _2)_2) forSome { type _2 >: IV with V; type _2 >: IV with V; type _2 >: IV with V; type _2 >: IV with V }
 cannot be applied to (java.lang.String, Int)
*/
}