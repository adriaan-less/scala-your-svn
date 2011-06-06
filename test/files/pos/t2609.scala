object Test extends App {
  def inject0[T]: T = null.asInstanceOf[T]
  def m0: String = inject0 // inject0[Nothing] is inferred

  def inject1[T](implicit m: Manifest[T]): T = {println(m.toString); null.asInstanceOf[T]}
  def m1: String = inject1 // inject1[Nothing] is inferred

  class M[T]; implicit def M[T] = new M[T]
  def inject2[T](implicit m: M[T]): T = {println(m.toString); null.asInstanceOf[T]}
  def m2: String = inject2 // inject2[Nothing] is inferred

  m0
  m1
  m2
}