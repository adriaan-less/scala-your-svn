class Scratch

import collection.generic.GenericCompanion

trait HasGenericCompanion[S[X] <: Traversable[X]] {
  def companion: GenericCompanion[S]
}

object HasGenericCompanion {
  lazy implicit val StreamHasCompanion: HasGenericCompanion[Stream] = new HasGenericCompanion[Stream] {
    def companion = Stream
  }

  lazy implicit val ListHasCompanion: HasGenericCompanion[List] = new HasGenericCompanion[List] {
    def companion = List
  }
}

trait Pure[P[_]] { // TODO: can we get this to work for Pure covariant in P?
  def pure[A](a: => A): P[A]
}

object Pure {
  implicit def HasGenericCompanionPure[S[X] <: Traversable[X]](implicit c: HasGenericCompanion[S]) = new Pure[S] {
    def pure[A](a: => A) = {
      c.companion.apply(a)
    }
  }
}

object Test {
implicitly[HasGenericCompanion[Stream]]

val pure = Pure.HasGenericCompanionPure(implicitly[HasGenericCompanion[Stream]])
val pure2 = Pure.HasGenericCompanionPure[Stream]

// this fails with:
//$anon.this.Pure.HasGenericCompanionPure is not a valid implicit value for this.Pure[Stream] because:
//ambiguous implicit values:
// both lazy value ListHasCompanion in object HasGenericCompanion of type => this.HasGenericCompanion[List]
// and lazy value StreamHasCompanion in object HasGenericCompanion of type => this.HasGenericCompanion[Stream]
// match expected type this.HasGenericCompanion[S]
//(fragment of Scratch.scala):43: error: could not find implicit value for parameter e: this.Pure[Stream]
implicitly[Pure[Stream]]
}