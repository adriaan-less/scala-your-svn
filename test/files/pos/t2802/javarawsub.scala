object Test {
  val hm = new MyHashMap() 
  hm.put("a", 1)
  /*error: overloaded method value put with alternatives (x$1: K,x$2: V)V <and> ((x$1: K with K,x$2: _2)_2) forSome { type _2 >: V with V; type _2 >: V with V; type _2 >: V with V; type _2 >: V with V } cannot be applied to (java.lang.String,Int)
  */
}