trait Zero
trait Succ[N]

trait ZipWith[N, S] {
  type T
  val x: T = error("")
}

object ZipWith {
  implicit def ZeroZipWith[S] = new ZipWith[Zero, S] {
    type T = Stream[S]
  }

  implicit def SuccZipWith[N, S, R](implicit zWith : ZipWith[N, R]) = new ZipWith[Succ[N], S => R] {
    type T = Stream[S] => zWith.T // dependent types replace the associated types functionality
  }
  
  val zw = implicitly[ZipWith[Succ[Succ[Zero]], Int => String => Boolean]{type T = Stream[Int] => Stream[String] => Stream[Boolean]}].x
}