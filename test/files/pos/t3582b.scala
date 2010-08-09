object ParamScoping {
  // how come T is properly scoped in the return type, but not in the body of the method
  def noOverlapFOwithHO[T, G[T]]: G[T] =  null.asInstanceOf[G[T]] 
  // def canAccessLaterFOinHO[A, B[x <: C], C]: G[T] =  null.asInstanceOf[G[T]]
  // def canAccessEarlierFOinHO[A, B[x <: A], C]: G[T] =  null.asInstanceOf[G[T]]  
}