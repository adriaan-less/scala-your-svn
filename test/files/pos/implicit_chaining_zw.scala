trait Zero
trait Succ[N]

trait ZipWith[N, S] {
  type T
  val x: T = error("")
}

object ZipWith {
  type ZWT[N, S, TT] = ZipWith[N, S] { type T = TT }
  implicit def ZeroZipWith[S]: ZWT[Zero, S, Stream[S]] = new ZipWith[Zero, S] {
    type T = Stream[S]
  }

  implicit def SuccZipWith[N, S, R, Z <: ZipWith[N, R]](implicit zWith : Z): ZWT[Succ[N], S => R, Stream[S] => Z#T] = new ZipWith[Succ[N], S => R] {
    type T = Stream[S] => Z#T // dependent types replace the associated types functionality
  }
  
  val zw = implicitly[ZipWith[Succ[Succ[Zero]], Int => String => Boolean]{type T = Stream[Int] => Stream[String] => Stream[Boolean]}].x
}